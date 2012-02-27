% show bars for accumulated rewards and accumulated punishments
function show_rewards_bar(wPtr, reward_acc, punish_acc)
	reward_x = 150;
	reward_y = 50;
	width_reward = reward_acc;
	reward_max = 900;
	width_reward_max = reward_max;
	height = 25;

	reward_box = [reward_x, reward_y, reward_x + width_reward, reward_y + height];
	reward_box_max = [reward_x, reward_y, reward_x + width_reward_max, reward_y + height];
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	gray = (white + black) / 2;
	color = [0, 0, 250];
	Screen('FillRect', wPtr, gray, reward_box_max');
	Screen('FillRect', wPtr, color', reward_box');

	punish_x = 150;
	punish_y = 80;
	width_punish = punish_acc;
	punish_max = 900;
	width_punish_max = punish_max;
	punish_box = [punish_x, punish_y, punish_x + width_punish, punish_y + height];
	punish_box_max = [punish_x, punish_y, punish_x + width_punish_max, punish_y + height];
	color = [250, 0, 0];
	Screen('FillRect', wPtr, gray, punish_box_max');
	Screen('FillRect', wPtr, color', punish_box');
	Screen(wPtr, 'Flip', [], 1);
end
