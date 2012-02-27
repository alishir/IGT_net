% action is in: {1,2,3,4}
% available_decks: binary representation of decsk, 1: there is some card in deck, 0: there is no card
function [reward, punish, finished_deck] = get_reward_punish(action)
	deck_size = 40;
  %  persistent decks;
    persistent decks = penalty_dist(deck_size);
	finished_deck = 0;
	if action == 1 || action == 2 || action == 3 || action == 4
		reward = decks.reward(action,decks.index(1,action));
		punish = decks.punish(action,decks.index(1,action));
		if decks.index(1,action) == deck_size
	%		disp('the last card in selected deck');
	%		disp(action);
			finished_deck = action;
		else
			decks.index(1,action) = decks.index(1,action) + 1;
		end
	else
		disp('invalide action selected ...!');
	end
end
