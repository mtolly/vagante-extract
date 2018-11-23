#!/usr/bin/osascript
on run
	display alert "Drop 'data.vra' or 'data' onto this app."
end run
on open file_list
	if (count of file_list) is 1 then
		do shell script "\"" & (POSIX path of (path to me as text)) & "Contents/MacOS/vagante-extract\" \"" & (POSIX path of item 1 of file_list) & "\""
	else
		display alert "Drop 'data.vra' or 'data' onto this app."
	end if
end open
