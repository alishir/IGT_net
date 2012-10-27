% This code is the implementation of IGT from Bechara et al(2000) paper:
% "Characterization of the decision-making deficit of patients with ventromedial prefrontal cortex lesions"
function igt_orig(contact, sid, save_path)
	[wPtr, wRect, old_pref] = init_screen();
	escape_key = KbName('ESCAPE');
	i = 0;
	decks = penalty_dist(40);	% 40 card in each deck
	acc_reward = 0;
	acc_punish = 0;
	max_itr = 102;
	game_seq = zeros(3,max_itr);	% sequence of card selection
	resp_times = zeros(max_itr,6);
	% shuffle decks
	shuffle_decks = randperm(4);
	show_decks(wPtr, decks, shuffle_decks);
	wait_for_start(wPtr);
	[pid, time_base] = start_eye_tracker(sid, save_path);
	itr = 1;
	while itr < max_itr			% iteration of card selection by subject
		is_deck_selected = 0;
	%	show_rewards_bar(wPtr, acc_reward, acc_punish);
		show_decks(wPtr, decks, shuffle_decks);
		[selected_deck, resp_time] = get_response();

		current_reward = 0;
		current_punish = 0;
		if selected_deck(escape_key)
			break;
		end
		% convert user selection to shuffle decks
		orig_sel = KbName(selected_deck);
		selected_deck = shuffle_decks(KbName(selected_deck) - 96) + 96;
		ind = selected_deck - 96;
		if selected_deck == 'a' || selected_deck == 'A' || selected_deck == '1'
			if (decks.index(1,ind) <= 40)
				is_deck_selected = 1;
			end
		end
		if selected_deck == 'b' || selected_deck == 'B' || selected_deck == '2'
			if (decks.index(1,ind) <= 40)
				is_deck_selected = 1;
			end
		end
		if selected_deck == 'c' || selected_deck == 'C' || selected_deck == '3'
			if (decks.index(1,ind) <= 40)
				is_deck_selected = 1;
			end
		end
		if selected_deck == 'd' || selected_deck == 'D' || selected_deck == '4'
			if (decks.index(1,ind) <= 40)
				is_deck_selected = 1;
			end
		end
		if is_deck_selected
			current_reward = decks.reward(ind ,decks.index(1, ind));
			current_punish = decks.punish(ind ,decks.index(1, ind));
			decks.index(1, ind) = decks.index(1, ind) + 1;
			resp_times(itr,:) = resp_time;
			game_seq(:,itr) = [selected_deck; current_reward; current_punish];
			itr = itr + 1;
			show_msg(wPtr, current_reward, current_punish, orig_sel);
			acc_reward = acc_reward + current_reward;
			acc_punish = acc_punish + current_punish;
		end
	end
	finished_him(wPtr);
	quit(old_pref);
	data_file = sprintf('%s/%d.dat', save_path, sid);
	save(data_file, 'game_seq', 'contact', 'time_base', 'resp_times');
end

function finished_him(wPtr)
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	gray = (white + black) / 2;
	screen_size = Screen('Resolution', 0);
	w_space = screen_size.width / 2;
	h_space = screen_size.height / 3;
	offset = 40;
	offset_w = 120;
	text_h = h_space * 1.5;
	text_w = w_space;
	Screen('TextFont', wPtr, char('Helvetica'));
	Screen('TextStyle', wPtr, 1);
	Screen('TextSize', wPtr, 30);
	Screen('DrawText', wPtr, disp('Game has been Finished'), text_w - offset_w, text_h + 80, black, gray);
	rect = [(screen_size.width / 2) (screen_size.height / 2) (screen_size.width / 2 + 20) (screen_size.height / 2 + 20)];
	Screen('FillOval', wPtr, [0,0,0], rect);
	Screen(wPtr, 'Flip', [], 1);
	system('killall ffmpeg');
	GetChar();
	Screen('FillOval', wPtr, gray, rect);
	Screen(wPtr, 'Flip');
end
function wait_for_start(wPtr)
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	gray = (white + black) / 2;
	screen_size = Screen('Resolution', 0);
	w_space = screen_size.width / 2;
	h_space = screen_size.height / 3;
	offset = 40;
	offset_w = 120;
	text_h = h_space * 1.5;
	text_w = w_space;
	Screen('TextFont', wPtr, char('Helvetica'));
	Screen('TextStyle', wPtr, 1);
	Screen('TextSize', wPtr, 30);
	Screen('DrawText', wPtr, disp('press SPACE to start'), text_w - offset_w, text_h + 80, black, gray);
	rect = [(screen_size.width / 2) (screen_size.height / 2) (screen_size.width / 2 + 20) (screen_size.height / 2 + 20)];
	Screen('FillOval', wPtr, [0,0,0], rect);
	Screen(wPtr, 'Flip', [], 1);
	GetChar();
	Screen('FillOval', wPtr, gray, rect);
	Screen(wPtr, 'Flip');
end


function [sec] = clock_to_sec(cl)
	base = 1;
	sec = 0.0;
	for i=0:5
		sec = sec + cl(6 - i) * base;
		base = base * 60;
	end
end

function [pid time_s] = start_eye_tracker(sid, save_path)
	params = sprintf('ffmpeg -f video4linux2 -s 320x240 -r 30 -i /dev/video0 -f oss -i /dev/dsp -f avi %s/%d.avi', save_path, sid);
	pid = system(params, [], 'async');
	disp(pid);
	time_s = clock()
end

function [wPtr, wRect, old_pref] = init_screen()
	AssertOpenGL;
	old_pref = Screen('Preference', 'verbosity', 0);		% contorl debuging checks
	screenid = max(Screen('Screens'));
	HideCursor();

	[wPtr, wRect] = Screen('OpenWindow', screenid, 0, [], 32, 2);
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	Screen('BlendFunction', wPtr, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	gray = (white + black) / 2;
	Screen(wPtr, 'FillRect', gray);
end

function quit(old_pref)
	Screen('CloseAll');
	Screen('Preference', 'verbosity', old_pref);
end

function show_decks(wPtr, decks, shuffle_decks)
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
	imgB = imread('./images/b.jpeg', 'JPG');
	imgC = imread('./images/c.jpeg', 'JPG');
	imgD = imread('./images/d.jpeg', 'JPG');
	textureA = Screen('MakeTexture', wPtr, double(imgA));
	textureB = Screen('MakeTexture', wPtr, double(imgB));
	textureC = Screen('MakeTexture', wPtr, double(imgC));
	textureD = Screen('MakeTexture', wPtr, double(imgD));

	if (decks.index(1,shuffle_decks(1)) > 40)
		imgA = imread('./images/blank.jpeg', 'JPG');
	end
	if (decks.index(1,shuffle_decks(2)) > 40)
		imgB = imread('./images/blank.jpeg', 'JPG');
	end
	if (decks.index(1,shuffle_decks(3)) > 40)
		imgC = imread('./images/blank.jpeg', 'JPG');
	end
	if (decks.index(1,shuffle_decks(4)) > 40)
		imgD = imread('./images/blank.jpeg', 'JPG');
	end
	
	textureA = Screen('MakeTexture', wPtr, double(imgA));
	textureB = Screen('MakeTexture', wPtr, double(imgB));
	textureC = Screen('MakeTexture', wPtr, double(imgC));
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


function [selected_deck, resp_time] = get_response(wPtr)
	key_is_down = 0;
	FlushEvents;
	tic
	while (toc < 3) && (key_is_down == 0)
		[key_is_down, secs, key_code] = KbCheck;
%		selected_deck = KbName(key_code);
		selected_deck = key_code;
	end
	resp_time = clock();
	while KbCheck; end
end

function mark_selected_deck(wPtr, selected_deck, shuffle_decks)
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
	shadow_offset = [5 5 5 5];
	deck_A = [pos_x, pos_y, pos_x + card_width, pos_y + card_height];
	deck_A_shadow_3 = deck_A + 3 * shadow_offset;

	deck_B = deck_A + [pos_x + card_width, 0, pos_x + card_width, 0];
	deck_B_shadow_3 = deck_B + 3 * shadow_offset;

	deck_C = deck_B + [pos_x + card_width, 0, pos_x + card_width, 0];
	deck_C_shadow_3 = deck_C + 3 * shadow_offset;

	deck_D = deck_C + [pos_x + card_width, 0, pos_x + card_width, 0];
	deck_D_shadow_3 = deck_D + 3 * shadow_offset;

	if (selected_deck == 'a' || selected_deck == 'A')
		img = imread('./images/a_sel.jpeg', 'JPG');
		deck_shadow = deck_A_shadow_3;
	elseif (selected_deck == 'b' || selected_deck == 'B')
		img = imread('./images/b_sel.jpeg', 'JPG');
		deck_shadow = deck_B_shadow_3;
	elseif (selected_deck == 'c' || selected_deck == 'C')
		img = imread('./images/c_sel.jpeg', 'JPG');
		deck_shadow = deck_C_shadow_3;
	elseif (selected_deck == 'd' || selected_deck == 'D')
		img = imread('./images/d_sel.jpeg', 'JPG');
		deck_shadow = deck_D_shadow_3;
	end

	texture = Screen('MakeTexture', wPtr, double(img));
	Screen('DrawTextures', wPtr, texture, [], [deck_shadow']);
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
	msg_text2 = disp(['But lost: ']);
%	msg_text = disp(['You won ', disp(reward), ' ,But lose ', disp(punish), ' from deck ', disp(deck)]);
%	Screen('DrawText', wPtr, msg_text, 300, 600, [0 0 156], [255 255 255]);
	white = WhiteIndex(wPtr); % pixel value for white
	black = BlackIndex(wPtr); % pixel value for black
	gray = (white + black) / 2;
%	Screen('TextFont', wPtr, char('sans-serif'));

	mark_selected_deck(wPtr, deck)
	Screen('TextFont', wPtr, char('Helvetica'));
	Screen('TextStyle', wPtr, 1);
	Screen('TextSize', wPtr, 30);
	Screen('DrawText', wPtr, msg_text1, text_w - offset_w, text_h, black, gray);
	Screen('DrawText', wPtr, disp(reward), text_w + offset_w, text_h, black, gray);
	if (punish == 0)
%		Screen('DrawText', wPtr, msg_text2, text_w - offset_w, text_h + offset, black, gray);
%		Screen('DrawText', wPtr, disp(punish), text_w + offset_w + 10, text_h + offset, black, gray);	% zero is right aligned
	else
		Screen('DrawText', wPtr, msg_text2, text_w - offset_w, text_h + offset, black, gray);
		Screen('DrawText', wPtr, disp(punish), text_w + offset_w, text_h + offset, black, gray);		% other numbers are left aligned
	end
	Screen('Flip', wPtr);
	tic
	while toc < 1.25 end;
end
