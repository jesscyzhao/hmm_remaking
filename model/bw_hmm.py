import numpy as np
import pandas as pd
import copy


class BWHMM:

    def get_error_matrix(self, sim_cluster, sim_eps, sim_num_state):

        sim_B = np.zeros((sim_num_state, len(sim_cluster)))
        for state in range(sim_num_state):
            this_state_ind = sim_cluster == state

            sim_B[state, :] = (1-sim_eps)/(len(sim_cluster) - sum(this_state_ind))
            sim_B[state, this_state_ind] = [sim_eps / sum(this_state_ind)]*sum(this_state_ind)

        return sim_B

    def get_switch_matrix(self, sim_num_state, Lambda):
        sim_A = np.zeros((sim_num_state, sim_num_state))

        sim_A[:, :] = (1-Lambda)/(sim_num_state-1)
        for i in range(sim_num_state):
            sim_A[i, i] = Lambda
        return sim_A

    def simulate_data(self, sim_A, sim_B, sim_num_obs):

        sim_num_state = sim_A.shape[0]
        sim_pi = np.array([1/sim_num_state] * sim_num_state)

        def draw_from(probs):
            return int(np.where(np.random.multinomial(1, probs) == 1)[0][0])

        observations = np.zeros(sim_num_obs)
        states = np.zeros(sim_num_obs)
        states[0] = draw_from(sim_pi)
        observations[0] = draw_from(sim_B[states[0], :])
        for t in range(1, sim_num_obs):
            states[t] = draw_from(sim_A[states[t-1], :])
            observations[t] = draw_from(sim_B[states[t], :])
        return observations, states

    def estimate_hmm_em(self, obs, init_A, init_B, criterion=1e-5):

        num_state = init_A.shape[0]
        num_obs = len(obs)
        init_pi = np.array([1/num_state] * num_state)

        done = False

        cur_A = copy.deepcopy(init_A)
        cur_B = copy.deepcopy(init_B)
        cur_pi = copy.deepcopy(init_pi)

        while not done:
            alpha = np.zeros((num_state,num_obs))
            c = np.zeros(num_obs)
            alpha[:, 0] = cur_pi.T * cur_B[:, obs[0]]
            c[0] = 1.0/np.sum(alpha[:, 0])
            alpha[:, 0] = c[0] * alpha[:, 0]
            # Update alpha for each observation step
            for t in range(1, num_obs):
                alpha[:, t] = np.dot(alpha[:, t-1].T, cur_A).T * cur_B[:, obs[t]]
                c[t] = 1.0/np.sum(alpha[:, t])
                alpha[:, t] = c[t] * alpha[:, t]

            # beta_t(i) = P(O_t+1 O_t+2 ... O_T | q_t = S_i , hmm)
            # Initialize beta
            beta = np.zeros((num_state, num_obs))
            beta[:, num_obs-1] = 1
            beta[:, num_obs-1] = c[num_obs-1] * beta[:, num_obs-1]
            # Update beta backwards from end of sequence
            for t in range(len(obs)-1, 0, -1):
                beta[:, t-1] = np.dot(cur_A, (cur_B[:, obs[t]] * beta[:, t]))
                beta[:, t-1] = c[t-1] * beta[:, t-1]

            xi = np.zeros((num_state, num_state, num_obs-1))
            for t in range(num_obs-1):
                denom = np.dot(np.dot(alpha[:, t].T, cur_A) * cur_B[:, obs[t+1]].T,
                               beta[:, t+1])
                for i in range(num_state):
                    numer = alpha[i, t] * cur_A[i, :] * cur_B[:, obs[t+1]].T * \
                            beta[:, t+1].T
                    xi[i, :, t] = numer / denom

            # gamma_t(i) = P(q_t = S_i | O, hmm)
            gamma = np.squeeze(np.sum(xi, axis=1))
            # Need final gamma element for new B
            prod = (alpha[:, num_obs-1] * beta[:, num_obs-1]).reshape((-1, 1))
            gamma = np.hstack((gamma,  prod / np.sum(prod)))

            newpi = gamma[:, 0]
            newA = np.sum(xi, 2) / np.sum(gamma[:, :-1], axis=1).reshape((-1, 1))
            newB = copy.deepcopy(cur_B)

            numLevels = cur_B.shape[1]
            sumgamma = np.sum(gamma,axis=1)
            for lev in range(numLevels):
                mask = obs == lev
                newB[:, lev] = np.sum(gamma[:, mask], axis=1) / sumgamma

            if np.max(abs(cur_pi - newpi)) < criterion and np.max(abs(cur_A - newA)) < criterion and \
                            np.max(abs(cur_B - newB)) < criterion:
                done = True

            cur_A[:], cur_B[:], cur_pi[:] = newA, newB, newpi

        return cur_A, cur_B, cur_pi

    def viterbi_decode(self, est_A, est_B, est_pi, obs):
        num_state = est_A.shape[0]
        num_obs = len(obs)

        alpha = np.zeros((num_state, num_obs))
        beta = np.zeros((num_state, num_obs))
        c = np.zeros(num_obs)
        delta = np.zeros((num_state, num_obs))
        phi = np.zeros((num_state, num_obs))

        alpha[:, 0] = est_pi.T * est_B[:, obs[0]]
        c[0] = 1.0/np.sum(alpha[:, 0])
        alpha[:, 0] = c[0] * alpha[:, 0]
        delta[:, 0] = copy.deepcopy(alpha[:, 0])

        # Update alpha for each observation step
        for t in range(1, num_obs):
            alpha[:, t] = np.dot(alpha[:, t-1].T, est_A).T * est_B[:, obs[t]]
            c[t] = 1.0/np.sum(alpha[:, t])
            alpha[:, t] = c[t] * alpha[:, t]
            for this_state in range(num_state):
                path_to_this_state = delta[:, t-1] * est_A[:, this_state] * est_B[:, obs[t]] * c[t]
                delta[this_state, t] = max(path_to_this_state)
                phi[this_state, t] = int(np.where(path_to_this_state == delta[this_state, t])[0][0])

        max_path = max(delta[:, num_obs-1])
        final_state = np.where(delta[:, num_obs-1] == max_path)

        best_path = [final_state]

        counter = 0
        for t in range(len(obs)-1, 1, -1):
            best_path.append(phi[best_path[0], t])
            counter+=1

        return best_path[::-1]

if __name__=="__main__":

    vanila_case = False
    moulton_case = True
    if vanila_case:
        model = BWHMM()
        sim_A = np.array([[0.85, 0.15], [0.12, 0.88]])
        sim_B = np.array([[0.8, 0.1, 0.1], [0.0, 0.0, 1]])
        sim_obs, sim_state = model.simulate_data(sim_A, sim_B, 1000)
        init_A = np.array([[0.5, 0.5],
                           [0.5, 0.5]])
        init_B = np.array([[0.3, 0.3, 0.4],
                           [0.2, 0.5, 0.3]])

        est_A, est_B, est_pi = model.estimate_hmm_em(init_A, init_B, sim_obs)

        print(sim_A)
        print(est_A)


    if moulton_case:
        model = BWHMM()

        num_obs = 1000
        num_ind = 50
        sim_num_state = 4
        sim_cluster = np.array([1, 3, 3, 1, 0, 0, 1, 1, 0, 2, 0, 2, 1, 1, 2,3, 3, 1, 3, 3, 3, 2, 3,1, 3, 2, 0, 1, 0, 3,
                                2, 3, 1 ,0 ,0, 3 ,1 ,2 ,1 ,0 ,1 ,3 ,2 ,2 ,2 ,3 ,2 ,2 ,0 ,0])
        Lambda = 0.6
        eps = 0.8

        sim_A = model.get_switch_matrix(sim_num_state, Lambda)
        sim_B = model.get_error_matrix(sim_cluster, eps, sim_num_state)
        sim_obs, sim_state = model.simulate_data(sim_A, sim_B, 1000)

        init_A = np.array([[1/sim_num_state] * sim_num_state] * sim_num_state)
        init_B = np.array([sim_B[i,: ][::-1] for i in range(sim_B.shape[0])])


        est_A, est_B, est_pi = model.estimate_hmm_em(sim_obs, init_A, init_B)

        print(sim_A)
        print(est_A)

        print(sim_B)
        print(est_B)

        # best_path = model.viterbi_decode(est_A, est_B, est_pi, sim_obs)
        # print(best_path)