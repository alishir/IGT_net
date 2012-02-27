% This code is the implementation of IGT from Bechara et al(2000) paper:
% "Characterization of the decision-making deficit of patients with ventromedial prefrontal cortex lesions"
function igt_orig(contact, sid, save_path)
	[wPtr, wRect, old_pref] = init_screen();
	escape_key = KbName('ESCAPE');
	i = 0;
	decks = penalty_dist(40);	% 40 card in each deck
	acc_reward = 0;
	acc_punish = 0;
	game_seq = zeros(4,100);	% sequence of card selection
	itr = 1;
	max_itr = 110;
	while itr < max_itr			% iteration of card selection by subject
		is_deck_selected = 0;
		show_decks(wPtr);
	%	show_rewards_bar(wPtr, acc_reward, acc_punish);
		[selected_deck select_time] = get_response();
		current_reward = 0;
		current_punish = 0;
		if selected_deck(escape_key)
			break;
		end
		if KbName(selected_deck) == 'a' || KbName(selected_deck) == 'A' || KbName(selected_deck) == '1'
			current_reward = decks.reward(1,decks.index(1,1));
			current_punish = decks.punish(1,decks.index(1,1));
			decks.index(1,1) = decks.index(1,1) + 1;
			is_deck_selected = 1;
		end
		if KbName(selected_deck) == 'b' || KbName(selected_deck) == 'B' || KbName(selected_deck) == '2'
			current_reward = decks.reward(2,decks.index(1,2));
			current_punish = decks.punish(2,decks.index(1,2));
			decks.index(1,2) = decks.index(1,2) + 1;
			is_deck_selected = 1;
		end
		if KbName(selected_deck) == 'c' || KbName(selected_deck) == 'C' || KbName(selected_deck) == '3'
			current_reward = decks.reward(3,decks.index(1,3));
			current_punish = decks.punish(3,decks.index(1,3));
			decks.index(1,3) = decks.index(1,3) + 1;
			is_deck_selected = 1;
		end
		if KbName(selected_deck) == 'd' || KbName(selected_deck) == 'D' || KbName(selected_deck) == '4'
			current_reward = decks.reward(4,decks.index(1,4));
			current_punish = decks.punish(4,decks.index(1,4));
			decks.index(1,4) = decks.index(1,4) + 1;
			is_deck_selected = 1;
		end
		if is_deck_selected
			game_seq(:,itr) = [KbName(selected_deck); current_reward; current_punish; select_time];
			itr = itr + 1;
			show_msg(wPtr, current_reward, current_punish, KbName(selected_deck));
			acc_reward = acc_reward + current_reward;
			acc_punish = acc_punish + current_punish;
		end
	end
	quit(old_pref);
	save('game_play.dat', 'game_seq');
end

function [pid time_s] = start_eye_tracker(sid, save_path)
	[pid, msg] = fork();
	% if I'm child
	if (pid == 0)
		params = sprintf('-f video4linux2 -s 320x240 -r 30 -i /dev/video0 -f oss -i /dev/dsp -f avi %s/%d.avi', save_path, sid);
		exec('ffmpeg', params);
	else if (pid > 0)		% I'm parent
		time_s = now();
	else
		disp('Error: could not fork()');
	end
end

function [wPtr, wRect, old_pref] = init_screen()
	AssertOpenGL;
	old_pref = Screen('Preference', 'verbosity', 0);		% contorl debuging checks
	screenid = max(Screen('Screens'));

	[wPtr, wRect] = Screen('OpenWindow', screenid, 0, [], 32, 2);
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	gray = (white + black) / 2;
	Screen(wPtr, 'FillRect', gray);
end

function quit(old_pref)
	Screen('CloseAll');
	Screen('Preference', 'verbosity', old_pref);
end

function show_decks(wPtr)
	screen_size = Screen('Resolution', 0);
	w_space = screen_size.width / 5;	% width space
	h_space = screen_size.height / 3;	
	pos_x = w_space / 5;		% || s |c| s |c| s |c| s |c| s ||	there are 5 space in horizon
	pos_y = h_space / 6;
	card_width = w_space;
	card_height = h_space;
	% general Deck
%	img = imread('./images/decker03.jpeg', 'JPG');
%	textureIndex = Screen('MakeTexture', wPtr, double(img));
	% deck with labels
	imgA = imread('./images/a.jpeg', 'JPG');
	textureA = Screen('MakeTexture', wPtr, double(imgA));
	imgB = imread('./images/b.jpeg', 'JPG');
	textureB = Screen('MakeTexture', wPtr, double(imgB));
	imgC = imread('./images/c.jpeg', 'JPG');
	textureC = Screen('MakeTexture', wPtr, double(imgC));
	imgD = imread('./images/d.jpeg', 'JPG');
	textureD = Screen('MakeTexture', wPtr, double(imgD));
	
	shadow_offset = [5 5 5 5];
	deck_A = [pos_x, pos_y, pos_x + card_width, pos_y + card_height];
	deck_A_shadow_1 = deck_A + shadow_offset;
	deck_A_shadow_2 = deck_A + 2 * shadow_offset;
	deck_A_shadow_3 = deck_A + 3 * shadow_offset;

	deck_B = deck_A + [pos_x + card_width, 0, pos_x + card_width, 0];
	deck_B_shadow_1 = deck_B + shadow_offset;
	deck_B_shadow_2 = deck_B + 2 * shadow_offset;
	deck_B_shadow_3 = deck_B + 3 * shadow_offset;

	deck_C = deck_B + [pos_x + card_width, 0, pos_x + card_width, 0];
	deck_C_shadow_1 = deck_C + shadow_offset;
	deck_C_shadow_2 = deck_C + 2 * shadow_offset;
	deck_C_shadow_3 = deck_C + 3 * shadow_offset;

	deck_D = deck_C + [pos_x + card_width, 0, pos_x + card_width, 0];
	deck_D_shadow_1 = deck_D + shadow_offset;
	deck_D_shadow_2 = deck_D + 2 * shadow_offset;
	deck_D_shadow_3 = deck_D + 3 * shadow_offset;
%	Screen('DrawTextures', wPtr, textureIndex, [], [deck_A', deck_A_shadow_1', deck_A_shadow_2', deck_A_shadow_3' ...
%												,deck_B', deck_B_shadow_1', deck_B_shadow_2', deck_B_shadow_3' ...
%												,deck_C', deck_C_shadow_1', deck_C_shadow_2', deck_C_shadow_3' ...
%												,deck_D', deck_D_shadow_1', deck_D_shadow_2', deck_D_shadow_3']);
	Screen('DrawTextures', wPtr, textureA, [], [deck_A', deck_A_shadow_1', deck_A_shadow_2', deck_A_shadow_3']);
	Screen('DrawTextures', wPtr, textureB, [], [deck_B', deck_B_shadow_1', deck_B_shadow_2', deck_B_shadow_3']);
	Screen('DrawTextures', wPtr, textureC, [], [deck_C', deck_C_shadow_1', deck_C_shadow_2', deck_C_shadow_3']);
	Screen('DrawTextures', wPtr, textureD, [], [deck_D', deck_D_shadow_1', deck_D_shadow_2', deck_D_shadow_3']);

	Screen(wPtr, 'Flip', [], 1);
end


% show bars for accumulated rewards and accumulated punishments
function show_rewards_bar(wPtr, reward_acc, punish_acc)
	reward_x = 150;
	reward_y = 50;
	scale = 1/5;
	width_reward = reward_acc * scale;
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
	width_punish = punish_acc * scale;
	punish_max = 900;
	width_punish_max = punish_max;
	punish_box = [punish_x, punish_y, punish_x + width_punish, punish_y + height];
	punish_box_max = [punish_x, punish_y, punish_x + width_punish_max, punish_y + height];
	color = [250, 0, 0];
	Screen('FillRect', wPtr, gray, punish_box_max');
	Screen('FillRect', wPtr, color', punish_box');
	Screen(wPtr, 'Flip', [], 1);
end


function [selected_deck secs] = get_response(wPtr)
	key_is_down = 0;
	FlushEvents;
	tic
	while (toc < 3) && (key_is_down == 0)
		[key_is_down, secs, key_code] = KbCheck;
%		selected_deck = KbName(key_code);
		selected_deck = key_code;
	end
	disp(secs);
	while KbCheck; end
end

function show_msg(wPtr, reward, punish, deck)
	screen_size = Screen('Resolution', 0);
	w_space = screen_size.width / 2;
	h_space = screen_size.height / 3;
	offset = 40;
	offset_w = 80;
	text_h = h_space * 1.5;
	text_w = w_space;

	msg_text1 = disp(['You won: ']);
	msg_text2 = disp(['But lose: ']);
%	msg_text = disp(['You won ', disp(reward), ' ,But lose ', disp(punish), ' from deck ', disp(deck)]);
%	Screen('DrawText', wPtr, msg_text, 300, 600, [0 0 156], [255 255 255]);
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	gray = (white + black) / 2;
%	Screen('TextFont', wPtr, char('sans-serif'));

	Screen('TextFont', wPtr, char('Helvetica'));
	Screen('TextStyle', wPtr, 1);
	Screen('TextSize', wPtr, 30);
	Screen('DrawText', wPtr, msg_text1, text_w - offset_w, text_h, black, gray);
	Screen('DrawText', wPtr, disp(reward), text_w + offset_w, text_h, black, gray);
	Screen('DrawText', wPtr, msg_text2, text_w - offset_w, text_h + offset, black, gray);
	if (punish == 0)
		Screen('DrawText', wPtr, disp(punish), text_w + offset_w + 10, text_h + offset, black, gray);	% zero is right aligned
	else
		Screen('DrawText', wPtr, disp(punish), text_w + offset_w, text_h + offset, black, gray);		% other numbers are left aligned
	end
	Screen('Flip', wPtr);
	tic
	while toc < 2.5 end;
end
