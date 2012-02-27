function show_decks(wPtr)
	pos_x = 250;
	pos_y = 150;
	width = 150;
	height = 217;
	img = imread('./images/decker03.jpeg', 'JPG');
	textureIndex = Screen('MakeTexture', wPtr, double(img));

	deck_A = [pos_x, pos_y, pos_x + width, pos_y + height];
	deck_B = deck_A + [200, 0, 200, 0];
	deck_C = deck_B + [200, 0, 200, 0];
	deck_D = deck_C + [200, 0, 200, 0];
	Screen('DrawTextures', wPtr, textureIndex, [], [deck_A', deck_B', deck_C', deck_D']);

	Screen(wPtr, 'Flip', [], 1);
end
