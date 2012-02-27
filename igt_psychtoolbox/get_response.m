function selected_deck = get_response(wPtr)
	key_is_down = 0;
	FlushEvents;
	tic
	while (toc < 4) && (key_is_down == 0)
		[key_is_down, secs, key_code] = KbCheck;
		selected_deck = KbName(key_code);
	end
	while KbCheck; end
end
