�]INSTALL - Customization for VEDIT
Copyright (c) 1985, 1987 CompuView Products, Inc.
Last Change: Ted Green - April 20, 1987
 Off Forward Forward & Reverse Blinking Underline Blinking Block Solid Block Attribute System No conversions Convert LC to UC Conditional convert Conditional reverse Strip I and O Strip O only Strip I only No stripping Str I/O, insert CTRLs Str O, insert CTRLs Str I, insert CTRLs No str, insert CTRLs On text only Anywhere (no fill) Anywhere (with fill) Physical lines per page Printed lines per page Left margin for printing Use form-feed for new page  Cursor Type Cursor blink rate; 5 is fastest Indent increment Upper/lower case conversion Conditional conversion flag character Delay for command mode scrolling Right margin for word wrap; 0 = Off High Bit (Bit 8) Processing (I=input, O=output) Cursor positioning option Horizontal scroll margin Horizontal scroll increment  Expand <TAB> key with spaces Auto-buffering in visual mode Auto-indent mode Point past text register insert Equate upper/lower case when searching MS-DOS end-of-file padding Reverse all upper & lower case keys Suppress (search) error handling Use explicit text delimiters Global file operations Justify paragraphs  Change tab positions Keyboard Input (0=BIOS Call, 1=System Call, 2=CCP/M) Begin in insert mode Hex code for status line character Hex code for screen continuation character Hex code for tab fill character Hex code for end-of-line character  Change Sign-on Message Pattern match character Command iteration left bracket Command iteration right bracket Command escape character Count for command mode help message Start up in command mode Speed of your computer in MHZ  Enable auto-startup (execute VEDIT.INI) Look for Help and VEDIT.INI on current drive Alternate drive for Help and VEDIT.INI files Size of free memory after auto-read (K bytes) Size of file buffering "page" (K bytes)  Number of screen lines Length of displayed line Line movement for paging Top line for cursor Bottom line for cursor Attribute for text character Attribute for screen erase Attribute for status line Attribute for status line messages Attribute for window border Attribute for window border message  Hex code for cursor character Hex code for screen erase character Screen line length Using high speed CGA/EGA board Attribute override for cursor (0 = auto) Address of screen Number of video board initialization bytes           MAIN MENU FOR VEDIT INSTALLATION
 Keyboard Layout
   1. Display or Print Keyboard Layout   2. Modify Keyboard Layout   3. Add Keystroke Macros to Layout
 Operational Settings
   4. Change Print Parameters (PP commands)   5. Change Edit Parameters (EP commands)   6. Change Edit Switch Settings (ES commands)   7. Change Visual Mode Interface   8. Change Command Mode Interface   9. Change File Handling Parameters
 Machine Related Settings
  10. Change Screen Display Parameters  11. Additional Memory Mapped Installation Features  12. Select CRT Terminal Type  Usage: INSTALL <input_file> <output_file> Unable to open  Misalignment in <INSTALL.INI> Error writing file. Check disk space.
 Insufficient Memory for INSTALL. 

      In response to the prompts for keyboard layout, press
     <RETURN> (same as <CTRL-M>) to keep current assignment, or a
     Function key - such as <F1> or <Ctrl-HOME> on an IBM-PC     Control character - such as <CTRL-U> or <BACKSPACE>     Escape sequence - such as ESC-H or ESC-1 on a CRT terminal
 You may "edit" the keyboard layout with the following keys:
     RETURN  Moves to the next function     -       Moves to the previous function     SPACE   Disables the function, i.e removes the assignment     B       Moves back to the first function
     D       Displays or prints the current layout     E       Skips the remaining prompts - EXIT to Main menu  

      If your CRT terminal or computer does not appear on the above list it must be added using the program INTMOD. First Exit the INSTALL program - being sure to SAVE any important changes. 
 Next, print the file NEWCRT.DOC.  It describes the procedure for adding your terminal to the menu.  When this is done, run INSTALL again on the VEDIT file you SAVED earlier, and select your CRT terminal using TASK 12.
  Display or Print Keyboard Layout Display Print com cmd %s%sThis option does not apply to your VEDIT.
   Supported Computers and CRT Terminals: 

 
Do you wish to select a CRT terminal (Y/N)?  
Type "H" if your terminal is not in the menu

 DEL 




 Please enter a number between   and  ESC 
Unable to open VEDIT.KEY
 FUNC- 
Enter the number of your terminal or %sto continue the menu  %sEnter a new message up to 64 letters long,
or %sto leave it the same:                    64->|

 

Load new keyboard layout from the file VEDIT.KEY (Y/N)?  
VEDIT.KEY has incorrect format
 

%sto leave Tab positions the same, or
Enter up to 33 new tab positions.  (You may use multiple lines.)

%stwice to finish entering tabs.

   or "E" to Exit Enter the option number None No Not Used Invalid character.  Please enter another one    Enter your selection number,        
%s%sto continue  
Enter Number of Video Initialization Bytes 
Enter Data Byte:  
Enter Port Address:  to select number  for previous menu):  or (or 


%sTurn on printer and adjust paper.
 Press <RETURN>  
Ok to save changes in %s (Y/N)? %s      The current Sign-on message is:

%s 

Please enter number(s) between %d and 255 separated by commas or spaces.
 %s      Current Tab Settings:

 

VEDIT Version Flags:
1H: 8080 MM
2H: 8086 MM
4H: Crt versions 
8H: MS-DOS
10H: CP/M-86
20H: IBM
40H: TI PC
80H: TRS 80
100H: Piiceon       Please enter Yes or No Yes 

Perform All New Keyboard Layout (Y/N)?  
*** Invalid input string ** 
Enter Delimiter to use 

Enter Control/Function Keys For The Following Functions...
 
%s%s%sKEYBOARD LAYOUT

 

Enter Function/Control key (<RETURN> when done):  
Enter key sequence followed by delimiter:
 
Undefined keystroke, try again.
 
Redefine Existing Key (Y/N)?  
Too many keystroke macros!!  Run INSTALL again to add more.
 
Macro is too long!
 
Press <RETURN> to keep current assignment.

Press "H" at any time for on-line Help.

 
*** Control sequence already used *** 

Upper/Lower Case Escape Sequences Equivalent (Y/N)?    �gs!~"݈!P �ͯm�"ň! �ͯm�DM! �ͯm�"�! �ͯm�"�! }2�}*�x�͂P�! }2�*���! 9^#V�! 9^#V��	�! 9��! ���`��"�  ��x��! �� ��*�"�!  }2��
! ���`��"�  ��x�?�! �� ��*��*��̓q��*����q�"�!$ �*��*�+�s!�*���`��"�  ��xʍ*��! �� ��! �"�ä! �*���<x"�*��ͯm�"�|�·Ö*��*��*��Ͱc���"�}*�}�*��*��ͥf����*�}��w��!  �! �� ��*�}�*���w·*���1o�*���[5�*�x^ �"�*�x ^ �"�*�������!�|��5�!  �! �! �!���!���*V}��� �! 9��"�}:���p*��r`i�*�}�́&��*�}����w�:����:�}o&  ��w��*�} Ͷw��*6}�͂P�!m��L@���*��͍)�����%:����*���`i�*x}�͂P��!o��L@��7��%*����8�:���4*����l�*��͔a�����w�'!  �! �� ��*��*��͆l���d:���d*��͔a�����w�\!  �! �� ��*����l�*��͔a��      �gs! 9^#V�~�! �! 9^#VBK`i~���`i^ !  ��w´`i^ !, ��w��!  �`i#DM+�s��`i#DM+^ !i#^ ! Ͷw��!���Ñ`i"�! 9^#VBK`i�*���wʫ`i~�ʣ�͏r�"�! 9^#V�^ �*����mx�Z*�  ��x�Z*�� ��x�Z! 9^#V��~#fo#��s#r!  �x�^!���*��! 9^#V�^#V��s!
 9^#V��~#fo#��s#r*��! 9^#V��s`i~�ʠ`i#DMÑè`i#DM��! �  �gs! "�!
 9^#VBK`i~���`i#DM+^ !. ��w��!  "���*�|��!. �`i#DM+�s! 9^#V��̓q���  �gs:�}o& +).}^#V�"�!
 9^#V�##^#V�! 9^#V�̓q��! 9^#V! ��w�b! 9^#V�*��ͯ��! �! 9^#V! ��wʴ! 9^#V�!��ͯ��!
 9^#V! ^#V�! 9^#V�̓q��! 9^#V�*��ͯ��!  �!  �!  �� ���SET   �gs! 9^#V�ͯm�"�*�|���*��!  �! �� ���  �gs! 9N#F*�x "	�:�}�ʏ*	�#"	�+^ �"�|�ʏ*�} Ͷwʏ*� Ͷw"�!  DM�L`i#DM`i�*��x�p!~�*	�#"	�+^ �ͤY���G!  DM�}`i#DM`iK ��xʌ�xð`i  ��xʰ!~�!
 �ͤY��`i+DMÏ�  �gsͽ& �  �gs���� ��� ��oe��  �gs!  DM��`i#DM`iK ��x����� }2�:�o& �  �gs���� � og����  �gs! 9^#V�)�|^#V�!b �͂P��!
 9~#��Y !
 9^#V�!g �͂P��!  ��N_��

%s %s   �gs!
 9^#V�}2�!��!
 ���o��:�o& DM! PYDM!  �`i��s`i ��w�� !  �!
 9^#V��s! �!��!
 9^#V�̓q���  �gs!
 9N#F! "e�!"�͂P�! 9^#V�*b}�!"�͂P���`i����w�0!*n}�*v}�*r}�*b}�!"�͂P�!
 9�Æ!`i����w�c!! 9^#V�*l}�*v}�*p}�*b}�!%"�͂P�! 9�Æ!`i����w�~!*T}�!8"�͂P��Æ!!="�͂P�! 9^#V�! 9^#V��(��"c�*c�����w�"*c�����w�"*c��! 9~#fo�lx��!*c��! 9~#fo�mx�"�*e��! 9^#V�! 9^#V�! 9^#V��@"�!
 9�!  "e�! |�!*c��

 %s%s 
%s%s %s%s 
%s   %s %s%s(%d)  %s:  :    �gs! 9~#��`"! 9^#V!����w�p"!~�! �ͤY���s"��%! 9^#V!����wʋ"!#�͂P�*>}�*b}�! #�͂P���! 9~#���"!
 9^#V�*@}�! 9^#V�!%#�͂P�! 9���"!
 9^#V�*@}�! 9^#V�!.#�͂P�! 9�! 9^#V!����w�	#*T}�!5#�͂P��*R}�*b}�!8#�͂P����    %s%s %xH%s%xH %u%s%u %s :  %s%s   �gs! 9^ ��cr��! 9s! 9^ �!0 ����wʓ#! 9^ !9 ��wʓ#! 9^ !���!
 9~#fo�_w�! 9^ �!A ����w��#! 9^ !F ��w��#! 9^ !���!
 9~#fo�_w�!���  �gs!  DM! 9^ ! ��w��#*B}DM�p$! 9^ ! ��w�$*:}DM�p$! 9^ ! ��w�%$!�$DM�p$! 9~��6$*\}DM�p$! 9^ !  �{x�p$! 9^ !  �zx�p$*�}DM! 9^ !@ �*�} �sx�ʂ$�!�$��&��Î$! 9^ ���%��CTRL-  CR  %s   �gs*�������! 9^#V�*b}�*<}�!U%�͂P�! 9�!  DM��$`i#DM`i�! 9~#fo�x�%`i)�!
 9~#fo^#V�`i�! 9~#fo�*b}�!`%�͂P�! 9���$!  �! 9^#V! 9~#fo+�! 9^#V�! 9^#V�!���*`}��� �! 9���%s%s   %s
 
%s%2d.  %s   �gs! 9^#V�*b}�!�%�͂P���ͱ}2g���}2h�}�ʞ%Ï%:g�o& ���#���%:g�o& � Ͷw��%*^}�! 9s#r�r%:g�o& �%s%s:    �gs! 9^ �!�%�*݈�͠P���*�}#"�}�%c   �gs!&�*݈�͠P��!  "�}�
   �gs!
 9^#V�!
 9^#V�*݈�͠P���! 9^#V���q��! 9^#V���q��++�*�}"�}�  �gs*v}�*b}�*d}�͂P���! ~��FX��  �gs! 9N#F!  "�}!  }2i�*�+����`i ��x��&`i ��x��&`i#)�|^#V! �*b}�*b}�!(�͂P�! 9�`ið'��@��'� )��E��'� )�hI��'! }2i�`i##)�|^#V�"j���'`i###)�|^#V�"j�! }2i���'*�} Ͷw�U'*b}�*<}�*2}�͂P����`&�! }2i�`i###)�|^#V! "j���'*�} Ͷwʎ'!
 9^#V�͍)�ê'*b}�*b}�*<}�*2}�͂P�! 9��`&��'��'�!��ڭ'!�'^#V��'�&�&�&�&�&�&�&�&''3's'`i ��x�(`i ��x�(:i�o& �*j��`i���͕:����

%s%s%s   �gs!�}"y�! �!l���j ��:l���S(! 9^#V!����w�J(!���! 9^#V��! 9^#V!����w�|(:l�o& _ ͶwE ��w�|(!���!
 9~#�(!l��͏r��!l����q�"u�*u�l�+DM`i^ !  ��wʺ(`i+DMä(`i^ !_ ͶwH ��w��(`i+DM!  "u�*y�##"y�++^#V�`i^ ��@#��"w�����w�)!���*w��*u�"u�`i+DM#l���w��(*u��  �gs*�xe "{�*{�#"{�+^ BKz�ʌ)*�} Ͷwʌ)`i ͶwDM!  "}��h)*}�#"}�*}�PY��xʌ)!~�*{�#"{�+^ �ͤY���a)�  �gs!����A*�*���*ň��.��*�����*��!
 9^#V�͍,��"��*������w�*! �*��+"��s �_w��v��s*�����V�! 9�*���! �!s �*�x��ZV�! 9�! }2�}*�"�*�|��8**�DM*�##^#V�"���1o��**����mZ��  �gs!�*�*�}�͙U���!
 9^#V��s#rz��s**�}�! �� ��! 9^#V�^#V�*ň��.��!	 �*�}�*ň��Vq���ʪ*!  �! �� ��! 9^#V�^#V�*ň��.��*ň�͏r�"߈�install.ini *CRT.TBL* r   �gs!  DM��*`i#DM`i�*߈�x�R+! ����"��x��+*��"�"��1+*���*�##�s#r*��"�!  �*�##�s#r! 9^#V�*���L.����**߈�  �gs! 9^#V�^#V! 9~#fo��w�5,! 9^#V�^#V�! 9^#V��~#fo#��s#r��6,��! 9^#V�^#V�! 9^#V��~#fo#��s#r��6,��! �! 9^#V���~#fo��s#r��%! 9^#V�~#��,! 9^#V�^#V�##^#V�!
 9^#V��s#r! 9^#V�~#��2,! 9^#V�^#V�##^#V�! 9^#V��s#r�\+�  �gs!
 9~#�ʁ,! 9^#V!
 �x�c,!~�!  �ͤY��!
 9^#V�^#V�!
 9^#V�!�,�͂P���� %d. %-30s   �gs*�|�¨,!  �! �� ��!  "��"��*�"��!
 ����*4}�͂P�!
 9^#V*���<x"��  ���w��,! ��,*�� ��w�*�� ���v�"��*�"��!  DM�-`i#DM*���*���`i���x�6-*��##^#V�"���-*��"��*���*��"��!���*���!���!���!���!����V+�! 9�*��"��*��"��*8}�͂P�*v}�*H}�͂P��! �!����j ��:��o& ��-*�������!}��5��`&!���!���͏r�"��*�� ���w��-*���!
 9~#fo��w��-*�����-�Ex H �-h �-�-*��|��.*�"��"��!  "��"��! |�·,�  �gs!
 9^#V�ͲX��!
 9�~#fo#��s#r�+�s{��K.�#.�  �gs!
 9^#V�*ň��.��*ň���q�#�����!
 9^#V��s#r*ň�!
 9^#V�^#V�̓q���  �gs!  "��*�x"��"��!  DMù.! PYDM`i ��x��.*��#"��+^ !> ��w��.*��#"��ñ.!
 9^ ! ��w�/*���!�/�͂P��! �*��"��!  �! �!  �!  �!  �*f}��� �! 9��"��!  DM�</`i#DM`i�*���xʧ/!> �*��#"��+�s*h}�͂P�! �!  ��(���*��#"��+�s!� �*��#"��+�s*j}�͂P�! �!  ��(���*��#"��+�s�7/*��))DMö/`i#DM`i ��x��/!  �*��#"��+�sñ/�%d   �gs!
 9^ ! ��wʈ0*�} Ͷw�A0! 9^#V! �_w$y^ �!
 9^#V! �_w#y^ �)))�x^#V�^ �#�!�2�͂P��Ä0! 9^#V! �_w$y^ �!
 9^#V! �_w#y^ �)))�x^#V�^ �!�2�͂P��! �! 9^#V! �_w$y^ �!
 9^#V! �_w#y^ �)))�x^#V�"��DM! 9^#V! �_w%y^#V�"��}2��! 9^ ��$1!� }2���?1!� "���?1*�x##^�}2���?1*�x#^�}2��*�x^ �+}2���?1�Ex  �0 �0 1 �0 1!1! 9^#V! �_w"y^ ! ��wʝ1!  �:��o& �:��o& �`i^ �!  �! 9^#V! �_w+y^#V��� �! 9��"����1!  �*���*���*��^#V�!  �! 9^#V! �_w+y^#V��� �! 9��"��! 9^ �æ2*���`i#DM+�s`i^ *���lx�2! �`is`i#DM`i^ *���lx�2*��+++�`is`i#DM`i^ *���lx�F2*�� �_w �͡w�`isõ2*�} Ͷw�\2*��+"��*���*��##"��++�s#rõ2! 9^#V! �_w"y^ ! ��wʘ2*���`isã2*���*���s#rõ2�Ex  �1 I2q2�%u %u   �gs!
 9^ ! ��w��2!+3�͂P�! �*�xDM*������*<}�*z}�͂P���*v}�*<}�*J}�͂P���!A �!����j ��:����*3!����̓q���*   �gs!  "��! "��! }2J�!
 9^ ! ��w�g3!~�!* �ͤY��! �*�x"��"�*�����*<}�*~}�͂P��!� }2�!  DMâ3*�#"�+`i#DM+:�o& �*�^ ���w��3`i  ��x��3!�4�͂P�`i ��w��3x���3��%*�^ �!�4�͂P��Ô3*v}�*v}�*P}�͂P���*��"�!P �!����j ��!J��!���!��!����q�! 9���d4:J�o& #�*|}�͂P���w4*���*��"���%�w4!  "���w4�w4�Ex ��34 G4 X4a4*��|��	4*��*����wʕ4!� �*��s�,  %d   �gs*�}�͂P�*�}�͂P�! �!� �! �!  �!  �!�4��� �! 9��"�}*�} Ͷw��4! }2�}�Enter a version number   �gs!  DM�5`i#DM`i)�! 9~#fo^#V�~��U5x��35��%`i)�! 9~#fo^#V�*b}�!V5�͂P����5�%s%s   �gs! 9N#F! }2�}:�}o&  ��wʄ5!� Ç5!  "�:�}o&  ��wʟ5!  â5! "�!U��*���v���s!  �!U���sͶs   �|t��s��Ti! 9�! �!M���Ͱc���! �!�}��Ͱc���*�}|���5͜4! �*ň��Ͱc���! �!ш��Ͱc���*ш+��Ͷw "ш*ш�*��*��<x"ш!ӈ�*ш�*M���v���s!  �!ӈ��s��s��Ti! 9�! �!�}��Ͱc���*�}�!�8�͂P��!  "K�Ò6*K�#"K�*K� ��x�K7*K�|�¶6*�} Ͷw¶6�H7*K� ��w��6*�} Ͷw��6�H7! �!S���Ͱc���*K� ��w�7*�} Ͷw�7*K�)))�x�*S��*��<x�*���v���s�67*K�)))�x�*S��*ш��v���s*K� ��w�H7*S�"׈Ë6!  "K��[7*K�#"K�*K� ��x�8*K�)))�x^#V�����*K�)))�x�s#r*K� ��wʤ7*�} Ͷw¤7�8*K�|�»7*�} Ͷw»7�8*K�)))�x~#��8!  �*K�)))�x��s��s��Ti! 9�*K�)))�x^#V�*K�)))�x^#V��Ͱc����T7!�x��s!�xͽs͑t��v"O�!�x��s!�xͽs͑t��v"Q�*Q�����"�x*�x"ǈ!Y��!  �!�x��s��s��Ti! 9����s*Q��*ǈ��Ͱc���"S�*O�"�x*O������"�x!  �!�x��s��s��Ti! 9�*O��*�x��Ͱc���*�x"Ɉ*Ɉ�*O�"ˈ*ˈ�"͈! }2�}�
VEDIT version: %d
   �gs! 9N#F*ˈ�*Ɉ�<x"a�*a��*�x�<x"_�*a�"�x!c��:�}o&  ��w�09!� �39!  ��v���s!  �!c���sͶs   �|t��s��Ti! 9�! �!�}��ͥf���!  "]��y9*]�#"]�*]� ��x�-:*]� ��wʡ9*�} Ͷw¡9�*:*]�|�¸9*�} Ͷw¸9�*:*]�)))�x~#��*:!  �*]�)))�x��s��s��Ti! 9�*]�)))�x^#V�*]�)))�x^#V�*]�)))�x^#V��ͥf������w�*:!  �! �� ���r9*_��*׈"׈!c��*�} Ͷw�[:!ӈ��sͶs   �|t�k:!ӈ��sͶs   �|t���s!  �!c���s��s��Ti! 9�! �!׈��ͥf����  �gs*�����!
 9^#V! �*b}�!�?�͂P���! 9~���:��%!  }2i�!  DM��:`i#DM`i �_w y^ ! 9~#fo��w��:��:`i"t�`i �_w y^ ! ��w��<`i �_w)y^#V*�}Ͷw��<`i �_w+y^#V�:i�o#}2i�+& #�!@�͂P���! "p�`i �_w$y^ �`i �_w#y^ �)))�x^#V�"j�"g�`i �_w!y^ ���<*g�~�ʳ;*�}ö;*Z}"g�*g��!@�͂P����<`i �_w%y^#V�*g�^ �)�x^#V�!@�͂P����<*g�^ ���#���<`i �_w"y^ ! ��w�/<*g�^ �!@�͂P���?<*j�^#V�!@�͂P����<`i �_w'y^#V�"p�`i �_w"y^ ! ��wʆ<*g�^ *p�� w�!@�͂P��Ü<*j�^#V*p�� w�!!@�͂P����<:i�o& �! ��`i �_w'y^#V�)�|^#V��Es�����<�Ex   �; �; �; B< U< < U< U<�<��%`i#DM�;!  �:i�o& �! �!���!���*V}��� �! 9��"v�*v�����w�7=! �*t�DM!  }2i�:i�o& �*v��zxʌ=`i �_w)y^#V*�}Ͷw�t=:i�o#}2i�:i�o& �*v���wʉ=`i#DM�C=`i �_w$y^ �`i �_w#y^ �)))�x^#V�"j�"g�! "p�!  "r�`i �_w%y^#V�"l�`i �_w'y^#V�"n�`i �_w!y^ ���?*g�~��>!  �>! �*g��s��?!  �*g�^ �`i �_w'y^#V�`i �_w%y^#V�)�x�`i �_w+y^#V�͝$�!
 9��*g��s��?!$@�͂P�`i �_w+y^#V��l%��*g��s��?`i �_w'y^#V�"p�`i �_w%y^#V�))�}^#V�"l�`i �_w%y^#V�))�}^#V�"n�`i �_w!y^ ! ��w� ?! "r�`i �_w"y^ ! ��w�]?*r��*n��*l��*g�^ *p�� w�!  �`i �_w+y^#V��� �! 9�*p��_w�*g��sá?*r��*n��*l��*j�^#V*p�� w�!  �`i �_w+y^#V��� �! 9�*p��_w�*j��s#r��?:i�o& �! ��`i �_w'y^#V�)�|^#V��Es�����?�Ex   > > p> �>  ? �>�?! |�:�

  %s%s

 %2d. %-52s %s %s %xH %xH %u %u 

   �gs! 9N#F*�}PY��x�K@!  ���%��4@�  �gs! DM! |���@*R}�*b}�! 9^#V�!�@�͂P�! 9�! �!x���j ��:x�o& _ Ͷw��@! �!  �x�ʰ@!  DM��%ý@!~�! �ͤY��*�}�! 9s#r��@�Ex N �@Y �@�@�W@�%s %s%s   �gs!  }2ց!  "�}! "~�! �! �! �!*}�*(}�͝$�!
 9�! ��w�HA*b}�*t}�͂P���`&!^C�!YC�͙U��"݈�PA*�����*b}�*b}�*b}�*�}�*݈�͠P�!
 9�*Ɉ DM�~A`i#DM`i�*ˈ�zx�#C*�++�*~����w²A*݈~��wʲA�`&*�����!��"��`i^ !� ��w��A`i#DM+^�*��#"��+�søA!� �*���s`i#DM*݈~��w�B*b}�!`C�*݈�͠P���!  "�}!  "��`i^ !� ��w�C`i#DM+^�}2ԁ`i#DM+^�}2Ձ:Ձ�ʪB*�� ��w�TB!- ���%�:ԁo& R ��wʂB:Ձo& T ��wʂB!hC�!cC��&��áB*ǈ�!ԁ�!����wC���!���!oC��&��! "����B*�� ��wʾB!- ���%�:ԁo& ���%�! "��*�} ��x�C��%*݈~��w��B*b}�!tC�*݈�͠P���!  "�}�B!  ��'@�!�����C���%*~�#"~��yA*݈~��w�UC! ���%�!���*݈���Z��*݈��mZ�!~"݈�XC�`&�LST: w %s <%s> RETURN [%s] %s   �gs! 9^#V�"ׁ*ׁ~���C*ׁDM*ׁ#"ׁ+~�ʥCÕC*ׁ�! 9^#V��Kq����C�!
 9^#V�̓q��! �*ׁ#"ׁ+~���C��CÈC!  �  �gs! 9^#V�"ف!�DM*ف^ !� ��w�OD*ف#"ف+^`is`i^ !^ ��w�GD*ف^ !^ ��w�@D*ف^ ! Ͷw�`is*ف#"ف`i#DM��C!  �`is!�DM`i~��xD`i#DM!� �`i��n& ��w��s!�DMÅD`i#DM`i~���D`i^�}2ہ:ہo& � Ͷw��D*F}�!�D��&��! �:ہo& Ͷw}2ہ:ہo& ���#�!  ���%�ÀD�%s   �gs!
 9^#V�^ !� ��w�E!
 9�~#fo#��s#r�+^�!
 9�~#fo#��s#r�+�s��D!� �!
 9^#V��s�  �gs! 9^#V�^ !� ��w��E*�x~��~E!
 9^#V�^ ��cr��!
 9^#V�^ ��cr����w�{E!  �àE!
 9^#V�^ �!
 9^#V�^ ���wʠE!  �! 9�~#fo#��s#r!
 9�~#fo#��s#r�3E! �  �gs*L}��L@���F!  �!�H��`��"2�����w��E*D}�͂P�ÌF!  ".��F! �*.���<x".�*.��ͯm�"4�|��$F�F*.��*4��*2��Ͱc���"0�*0��*.���w�XF*Ɉ�*0��*͈�lx�eF!  �! �� ��*4�^ !� ��wF*4�#^ ! ��wʐF*N}�͂P��`&�*4�"8�*Ɉ":�!  DMéF`i#DM`i�*0��zx��F*8�#"8�+^�*:�#":�+�s ! ��w��FäF*ɈPY"ˈ�*�}��L@���F*Ɉ "ˈ*�}��L@��*�x�s*�}�͂P�*�}�͂P�!  DM!���!���`i#DM+���H���ʫH��%!���!�H��&��!  ��'@�!d��!���ͯK��",�|��iG!d����C��|G*\}�!�H��&��*ˈ",�!4 ��'@�!<���lN���G!6��!d��!<���UL���ʯG*�}�͂P��1G*,���M�!<��!���*,���~M���èH:<�o& �uH*,���M�!d��!���ͯK��",�|���G!d����C��
H*\}�!�H��&��èH! PY�<xDM`i  ��x�(H!  DMèH!  DM*�����èH�`i+DM!�|��5���%�`&*�����èH`i+DM��@èH`i+DM*�}�͂P�èH�Ex  �G  �G- HB +HD ZHE ;HH <Hb +Hd ZHe ;Hh <HeH�G�VEDIT.KEY [%s] %s %s   �gs*ǈDM!  "܂*܂#"܂+�! 9~#fo�x�I`i#DM+~���H��H`i~��I!  �`i#DM+~��I�I��H`i~��I!  ��! 9^#V�̓q��`i#DM+~��:I�,I�! 9^#V�̓q��!� �! 9^#V���q��! 9~#fo�s! �  �gs*�}��l%�}2ނ*�}�͂P�!���lN�"�*�|�°I:�o&  ��wʥI�*�}�͂P��zI!߂�!	��!���UL�����I*�}��L@���I*߂��M���I�zI*�}�͂P�!	�DM!  "	�:ނo& �!	���xJ��"�|��UJ*�����w�zI!	�����D��! "	�`i^ !� ��w�;J`i#DM�%J`i����{x�RJ*�}�͂P��zI��I*	�|��oJ!��!	��*ˈ��~M���! |��zI�  �gs!  "�! |�ʤK*��!���dO��"�*�|��.K!
 9^ �:�o& ���wʽJ!  �:�o&   ��lx�.K:�o&  ��zx�.K:�o& �!�K�͂P��:�o�!
 9�~#fo#��s#r�+�s!  �!
 9�~#fo#��s#r�+�s!� �!
 9^#V��s! �!� �*���s!;��!
 9^#V�!���UL���"�*� ��wʇK! 9^#V�#^ �!
 9^#V�^ �!�K�͂P���! �*�|�K*�}�͂P�!���*�"�ÄJ�%c \%c%c\   �gs*Ɉ "?�"=�*?��*ˈ�zx�QL*?�"=�*?�#"?�+^ !� ��w��K*?��*ˈ�lx��K!  ���K! 9^#V�*?���-E���&L*=��! 9^#V���D��*=��*?�#"?�+^ !� ��w�NL*?��*ˈ�lx�KL!  ��&L��K!  �  �gs*Ɉ#DM`i�*ˈ�zx��L`i"A�`i#DM+^ !� ��wʗL`i�*ˈ�lxʔL!  ��rL*A��!
 9^#V��-E����L*A��! 9^#V��s#r�! 9^#V���D��! 9^#V�*A���-E����L! �! �`i#DM+^ !� ��w��L��L�aL!  �  �gs! 9^#V*ˈ��w�M�! 9^#VBK`i#DM+^ !� ��w�6M�!M`i#DM+^ !� ��w�KM�6M! 9^#V�"C�`i�*ˈ�zx�wM`i#DM+^�*C�#"C�+�s�VM*C�"ˈ�  �gs!  DM`i#DM+�! 9~#fo^ !� ��wʨMÉM!  "E�*E�#"E�+�!
 9~#fo^ !� ��w��MîM*ˈPY�*E�"K�*K��*͈�lx��M*�}�͂P�!���*K�"I�*ˈ"G�*G��! 9~#fo�lx�)N*G�+"G�^�*I�+"I��s�N*K�"ˈ! 9^#V�!
 9^#V���D��`i�! 9�~#fo��s#r!
 9^#V�!
 9^#V���D���  �gs!  "M�*M��!
 9^#V��dO��"O�*M�|���N! 9^#V�^ ! ��w��N! 9^#V�^ !  �lx��N! 9^#V�^ ! �zx��N! 9^#V�^ !^ ��w��N!� �!
 9^#V�#�s!  �! 9^#V*M�^ ! ��w�#O!� �!
 9^#V*M��s*M��!� �!
 9^#V*O��s*O� ��x�EO!  �! 9^#V*M����C�*O�"M�! |��xN�  �gs! 9N#F!  }2Q�! }2R�ͱ�`i�! 9~#fo�s:Q�o& �`i�! 9~#fo��n& ��w��s!  }2Q�`i�!
 9~#fo^ !^ ��w��O!^ �! 9�~#fo#��s#r`i�s`i�!
 9~#fo^ ! ��w�*P!^ �! 9�~#fo#��s#r�+PY�s!Z �`i�! 9~#fo�s:R���LP`i�!
 9~#fo~��LP!� }2Q��[P!
 9�~#fo#��s#r!  }2R����`i�! 9~#fo�s{�O!
 9^#V��  �gs!
 9�!
 9^#V�!�Y���P����  �gs! 9^#V�"S�! 9�! 9^#V�!�P���P����  �gs*S��!
 9^#V�ͤY���8��gs!� 9N#F!  "Y�!� 9^#V�"W�!� 9�~#fo#��s#r�+^ �"U�|���T*U�% ��w��T!  �! 9s! "[�!  "]�!'"_�!� 9^#V�^ �"U�- ��w�yQ!  "[�!� 9�~#fo#��s#r�+^ �"U�*U�0 ��wʣQ!0 "]�!� 9�~#fo#��s#r�+^ �"U�*U�* ��w��Q*W�##"W�++^#V�"a�!� 9�~#fo#��s#r�+^ �"U��"R!  "a�!� 9�~#fo#��s#r�+^ �"U�i#^ ! Ͷw�"R*a�
 �_w�*U���"a���Q*U�. ��w��R!� 9�~#fo#��s#r�+^ �"U�* ��w�{R*W�##"W�++^#V�"_�!� 9�~#fo#��s#r�+^ �"U���R!  "_�ÜR!� 9�~#fo#��s#r�+^ �"U�*U�i#^ ! Ͷw��R*_�
 �_w�*U���"_�ÄR! "c�*U�l ��w��R!� 9�~#fo#��s#r�+^ �"U�! "c��S*U�h ��w�S!� 9�~#fo#��s#r�+^ �"U�*U�ïS! "e��FS!
 "e��FS! "e��FS!��"e�*c��! 9�*e��*W����T�! 9��"g�*c��*W�"W���S*W�##"W�++^#V�"g����q�"c���S*W�##"W�++^#V�"U�*U��! 9"g��s��S�Ex c �Sd @So %Ss rSu .Sx 7S�S! 9�*g��<x"c�*c��*_��x��S*_�"c�*[�|��-T�T*Y�#"Y�*a�+"a�#�*c��x�-T*]��`i�Es�����w�*T!�����S!  "e��=T*e�#"e�*g�~��tT*e��*_��x�tT*g�#"g�+^ �`i�Es�����w�qT!����6T*e��*Y�"Y�*[�|�½TÑT*Y�#"Y�*a�+"a�#�*c��xʽT!  �`i�Es�����wʺT!���ÊT��T*U��`i�Es�����w��T!���*Y�#"Y���P*Y���!  "�}"�}!
 9~2�}G+V+^+�6 "�}�+N+V+^!�}w#�Uy��=U/<O*�}�}~���>U:�}G!�}> �w#�1U�>U�!�} >�^�)|��RUg,�JU�s+�=�EUZ !�U~*�}+"�}w!�}���>U#�tU*�}��U+6-��0123456789abcdef  �gs�W\DM|�­U!  ��! 9^#V�! 9^#V���U����  �gs! 9^#V��mZ�!�}DM��U`i DMx���U!��"�!  �!
 9^#V���Kq���
V��U! 	^#V�!
 9^#V��`��"i�����w�/V!  �*i��! 9^#V! �s! �! 9^#V! �s! 9^#V��  �gs! 9N#F!  "k��wV*k�#"k�*k��! 9~#fo�x��V!
 9^#V�"o�ÞV*o�+"o�*o�|���V! 9^#V�ͲX�"m�����w��V*k��*m��`i#DM+�s×V�pV*k��  �gs! 9N#F!���`i ��nͶw��s! 	^ ! Ͷw�W!������Z���W!����\W! 9^#V! ��w�\W`i~#��\W!
 9�`i^#V�! 	^#V��<x��v�t����s͑t���s!  �! 	s#r`is#r! 9^#V�! 9��s��s! 	^ ��Ti! 9�Ͷs    ́vʟW!���!  �  �gs! 9N#F!w��! ���s    ��s! 	^ ��Ti! 9����s! 	^ ! Ͷw�X!w��! 	^#V�`i^#V��<x��v�t����s�|t���s�?X`i~#��?X!w��`i^#V�! 	^#V��<x��v�t����s͑t���s!w���s�  �gs! 9N#F�ͲX�"{�����wʮX! �*{�Ͷw"{�ÛX`i�~#fo+��s#r! �`i ��n��w��s!����SX�Ex   �X �X tX�X*{��  �gs! 9N#F! 	^#V�`i^#V��lx�sY! 	^ ! Ͷw��X!���!���`i ��nͶw��s`i ~#��Y�͠\�!	 	^#V�! 	^#V�! 	^ �Ͱc���"}�  ���w�[Y*}�|��DY! �GY! �`i ��n��w��s!���! 	^#V`is#r*}��! 	s#r`i�~#fo#��s#r�+^ !� Ͷw�  �gs!~�!
 9^#V�ͤY���  �gs! 9N#F`i
 ��w��Y!
 9^#V�! ���Y������w��Y!���!
 9^#V����Y���  �gs!
 9N#F! 	^#V�`i^#V��lx�Z! 9^#V!� Ͷw����Z���! 9^#V�`i�~#fo#��s#r�+�s !� Ͷw�  �gs! ~DM`iy~��zx�lZ`i DM����mZ��LZ�  �gs! 9N#F!  "�`i ~���Z! 	^ ! ͶwʨZ!������Z��"�! 	^ �͔a��*���w"�! 	^ ! Ͷw��Z! 	^#V��1o�!  �! 	s*��  �gs! 9N#F!AZ"�~! 	^ ! Ͷw�[!���! 	^ ! Ͷw�|[! 	^#V�`i^#V��<x"��*���! 	^#V�! 	^ �ͥf�������w�|[! �`i ��n��w��s!  �`is#r! 	s#r!���!
 9^#V!����wʳ[!���`i ��nͶw��s!  �`is#r! 	s#r!  �`i ~#���[�͠\�!	 	^#V! ��w��[! �! 9�! 	^ �ͥf�������w�U[!
 9^#V��! 	^#V`is#r!	 	^#V�! 	^#V��! 	s#r! �`i ��n��w��s!
 9^#V�`i�~#fo#��s#r�+�s !� Ͷw�  �gs! ~DM`i ~�ʆ\`i DMy~��lxʃ\!  ��b\!  �`is#r! 	s#r! 	s#r`i�  �gs! 9N#F! 	^ ��l���\! �!	 	s#r`i �! 	s#r�! �ͯm�"��|�ʼ\! �!	 	s#r! �`i ��n��w��s*���! 	s#r�!�	 s#y��]* � �"�*�~"��E]�~�   � �4]  �gs! �!ͅ�!� ��r���!  �:� o&  Ͷwͅ�s!)_"��!ͅDM! "M�*M� ��x�_`i^ !  ��w­]`i^ !	 ��wʵ]`i#DMÑ]`i~��_`i^ !> ��w��]! "Q���]`i^ !< ��w��^!  "Q�`i#DM^ !  ��w�^`i^ !	 ��w�	^��]`i"O�`i#DM~��F^`i^ !  ��w�4^`i^ !	 ��w�C^!  �`i#DM+�s�F^�^*Q��͔a�*Q�|��i^!��*O����_��"Q��y^!  �*O���`��"Q�*Q�����w��^!*_�!� �̓q��*O��!� �͟q��!L_�!� �͟q��!� �!	 ���o��!
 ��N_��_`i�*M�#"M�+)���s#r`i#DM~��_`i^ !  ��w��^`i^ !	 ��w�_!  �`i#DM+�s�_��^Ä]!���*M�����!  ��N_�� Can't open file for redirection:  $   �gs*�~�Es!  DM`i ��x�y_`i#DM+�͔a��__! 9~#�ʜ_! ���o� Ͷwʜ_!�_���l��4]�A:$$$.SUB   �gs!��"�!���  �gs!  �con: CON: lst: LST: prn: PRN: pun: PUN: rdr: RDR:   �gs!
 9^#V�!�! 9^#V��`����  �gs!�~"S�!  "W��=`*S� "S�*W�#"W�*W� ��x�``*S� ^#V!�_��w�j`�,`!��"�!���!DM�z``i DM`i~#�ʛ`! 9^#V�`i^#V��Kq��ʛ`�r`! 	^#V�"U�!
 9^#V! Ͷw#"Y�*Y� Ͷw��`*U�^�*S��s{���`!��"�!���*Y� Ͷw�a*U�#^�*S�#�s{��a!��"�!���! 	^#V�*S� �s#r*U�##^�*S�##�s*U�###^�*S�###�s!�_�*S� �s#r�*S��! 9^#V�! 9^#V�! 9^#V�*U� ^#V��Es�!
 9�!  �xʐa!�_�*S� �s#r!���*W��  �gs! 9^#V!  �xºa! 9^#V! �x��a!��"�!���! 9^#V�)))�~DM! 	^#V�! 	^#V��Es��! 9s#r!  �! 	s! 	s! 	s`is!�_�! 	s#r! 9^#V��  �gs![�DM�1b`i' DM`i����zx�Kb`i% ~��Ub�)b!��"�!����!
 9^#V���o��"������w�yb!��"�!���*��� ��wʋb�q"��*����&q�!
 9^#V! Ͷwʭb�! ���o���! ���o��� ��w��b!
 9^#V! Ͷw��b�! ���o��� ��w��b!��"��?q!����c!
 9^#V! Ͷw ��w�c!��"��?q!���!  �!! 	s#r!# 	s!$ 	s*���!& 	s`i�! 9^#V! �s#r!
 9^#V! Ͷw#�!% 	s!�c�! 9^#V! �s#r!
 9^#V! Ͷwʂc�͵j��?q!  �  �gs! 9N#F�kk�! ���o��!  �!% 	s!  �  �gs! 9^#V�)))�~DM! 9^#V�! 9^#V�! 	^#V�`i^ �)W^#V��Es����  �gs!  DM! 9^#V�"��*��& ^ ��&q�*��$ ~��pd*��$ ^ �!� ���<xDM�! 9~#fo�{x�Sd! 9^#VBK�! 9^#V�*����e����pd�?q!  �! 9^#V`i�<x �͡w"��|���d*���! 9^#V`i�*����+l���"��|���d�?q*���*���<x �͑wPY�*�� �͑wPYDM`i�! 9~#fo�zx�e! 9^#V`i�<x�! 9^#V`i�*����e����e�?q`i��?q! 9^#V��  �gs! 9N#F��xk��4e!���! 9^#V�! 9^#V�!$ 	^ !� ��r���!$ 	^ ! 9~#fo Ͷw�!$ 	s{�e`i! �~#fo#��s#r!  �  �gs:�����e!� }2��!  }2��}2��!���!
 ���o��!
 �! ���o��:��o&  ��w��e!  }2��!  �:_o�:��o#}2��& ��#�s! "��:��o& DM�! 9~#fo�x�f! 9^#VBK�! 9^#V�*������r���`i�*��"��`i�:��o��<x}2��`i�  �gs!
 9N#F!  "���lf*��#"��*���! 9~#fo�xʡf! 9^#V���o��`i#DM+�s ! ��w¡f�ef*���  �gs! 9^#V�)))�~DM! 9^#V�! 9^#V�! 	^#V�! 	^ �)`^#V��Es����  �gs!  DM! 9^#V�"��*��& ^ ��&q�*��$ ~��gg*��$ ^ �!� ���<xDM�! 9~#fo�{x�Jg! 9^#VBK�! 9^#V�*����h����gg�?q!���! 9^#V`i�<x �͡w"��|���g*���! 9^#V`i�*����3l���"��|���g�?q*���*���<x �͑wPYDM|���g!���`i�*�� �͑wPYDM`i�! 9~#fo�zx�h! 9^#V`i�<x�! 9^#V`i�*����h����h�?q`i��?q! 9^#V��  �gs! 9N#F��xk�  ��x�?h!���! 9^#V�!$ 	^ !� �! 9^#V��r����!" ���o��"�|��wh!���!$ 	^ ! 9~#fo Ͷw�!$ 	s{�©h`i! �~#fo#��s#r!  �  �gs!
 9N#F! 9^#V�"��*��+"��#|��i`i^ !
 ��w��h! �! ���o��`i#DM+^ �! ���o����h! 9^#V��  �gs!
 9N#F! 9^#V�"��*��+"��#|��Ki`i#DM+^ �!
 9^#V���o���$i! 9^#V��  �gs! 9^#V�)))�~~��|i!��"���s�����! 9^#V�)))�~^#VBK! 9^#V��-j!& 	^ ��&q�!� �`i ��n��w��s�! ���o��! �`i ��nͶw��s�͵j��?q!
 9�!! 	^#V���vͶs   ��t��s!$ 	^ ���v��s�|t�t����s�|t���s�@j!��"���s������Ex   j �i �ij!  �!# 	s!
 9��sͶs    ́v�{j!  �!! 	s#r!$ 	s!��"���s�����!
 9��s��v Ͷw�!$ 	s!
 9��sͶs   �u��v�!! 	s#r!
 9��s�  �gs! 9N#F�!# ���o��`i! ~#���j!  �!$ 	s!  �`i! �~#fo+��s#r��xk��k!���! "��*��� ��{x�5k*��+"��^ ! ��w�2k*��#"���5k�k*�����!$ 	s !� ��w�gk`i! �~#fo#��s#r!  �!$ 	s!  �  �gs!  "���  �gs! 9N#F*��PY��w k!! 	^#V*����w�l!� �! ���o���!! ���o��"� ��w��k*� ��w��k!  "�! �!� �!� ���q���!  "��! �*�|���k!���`i"��!! 	^#V�"��!  �  �gs! 9^#V�)))�~^ ��ͅs!�8lͅs"�*��!� "�� ��*��� ��ql*�! 4�cl#4*�+"�}��9l���ʁl�ʁlo& "��*�����gs! 9�!F 9^#V���o��DM! 9�!H 9^#V���o����&q�! 9�! ���o��� ��w��l! 9�! ���o��!��"���l! 9�! ���o���?q����gs! 9�!2 9^#V���o��DM��&q�! 9�! ���o��DM�?q`i� ��w�>m!��"�!���!  �  �gs! 9N#F!��	^#V�+))"����1o�!
 9^#V�ͯm�"��|�ʫm*��PY��wʫm!
 9^#V*���{xʗm*��ßm!
 9^#V��*�����r���*���  �gs! 9^#V�### �͡w#"��*��"��|���m!��"��"��"��*��##^#VBK��m`i"��! 	^#VBK! 	^#V�`i���w�Yn! 	^#V�`i^#V�))PY���w�Yn! 	^#V�^#V�`i��~#fo��s#r! 	^#V�##^#V! 	s#r��m`i^#V*���lx��n`i^#V*����wʉn! 	^#V�*��##�s#r��n*��))PY"��*���*��##�s#r! 	^#V�*��##�s#r`i^#V*���<x�*���s#r*���`is#r*��"��!  �! 	s#r`i �`i�*����w�-o! ���p�DM����w�	o!  �! �`is#r!  �! 	s#r`i ��1o�*��DM��m�  �gs! 9^#V!��"��*��##~#��To!���*��DM�eo! 	^#VBK`i�*���{xo! 	^#V�*�����{xʸo! 	^#V�`i���lxʵo*��PY��{x¸o! 	^#V�*�����zx¸o�\o! 	^#V�*��##�s#r*���! 	s#r`i"��!  �ͅs��o��ͅs*�DM*��� �o�g��! 9N#F#^#V
� �p�	�p�pkb6 #>6 #=�p>6 #=�$p�� 
��p�Ep�0_z������W�/p�/�Op��Rp��
�:> �}p
��Aڸp�[�np�@�zp�aڸp�{Ҹp�`w#
�.ʘp�ʱpʀpͿpw#Áp{�o|� g
�ʱpͿpw#¢p& jz���!��|����*��p>?��a��{�� ��0��:��p��7�! 9^#V*�~�q�*�}�|��q*�~�"�~�|��!���=�! 9}�_|#�W�"��ͅs �� o& ��ͅs �� 2h:���� _� ͅs :h_� ! 9���_q! 9�F+N+V+^+~+ng�x��}q��}q��}q#�hq�o�g��! 9V+^+~+ng�w�ʛq#Ðq�|��! 9��óq! 9�F+N+V+^+~+ng対��q#ýqx���qw���q#��qw��|��! 9~#fo  ����q#��q�}���! 9^#V#N#F#n�x��rs#�r���!	 9F+N+V+^+~+ng��Mr�1r}��Mr�ar	�	���Ar�+����+wx��Ar�ɯ��Wr����~#x��Wr��! 9~�a�tr�{�tr� o& ��! 9~�Aڊr�[Ҋr� o& ��  �gs! 9N#F`i^ !  ��w¸r`i^ !	 ��w��r`i#DMÜr!  "È`i^ !- ��w��r! "È`i#DM��r`i^ !+ ��w��r`i#DM!  "��`i^ !i#^ ! Ͷw�0s*��
 �_w�`i#DM+^ ���"����r*È|��As*���2x�Ds*������DM!  9�9������`i�Es���������|����DM!  9�9��!_s�`i�~#x��zs��! 9���s�������~#s!�s������s����|���! ������s�! ����~#~#~#~���w#w#w#w��!��*���*�������"���"����*���*��"���"��*���*��"���"���!���> �w#�*t�!��~��{v#�9t�pv�=�ɯ<�����!���O~�mt���!����Ft�Jt+�kt������!����#t�����!����#t�����!���#¯t�����!���#��t�����!���#��t��!��~/w#��t�:���?�*���*��)�)��u,=��t"���"����u:���:���?�5uW!����~w+�)u�"u��! Ny����'t����7u����7uy���ͣu���$t�����7uy�����7uͣu��!��w#�yu���$t��ͣu���ͣu��!��w#u��!���w#©u> �!���~�w#¸u��O��!����#��uy� ��u!��4�=±u��=�v�!���~�w#��u�O��!����#�vy� ��u��u��!����#� v��!����~6 #�3v> �!���~�w#�Fv�cv��!����#�Zv�=�?v���Nt�{v!  ���Nt�pv! �<��Nt�{v�pv�Nt�{v�{v�pv�Nt�pv�{v�Nt�pv�pv�{v�bt�{v�pv�bt�{v�{v�pv�bt�pv�{v�bt�pv�pv�{v"��!  "���"��|���v!��"���*���z���w���2x}���*w�}��z��w��2x|��|��w/g}/o#z��*w/W{/_�MD!  �7w�}��y/Ox/G>)�)��Hw,	�Yw�}�o|�g�=�@w�=�@w��DM!  >)�)��ow	=�gw�}���{�_ʞw|��w|7g}ow���{�_ʞw)w}���{�_ʞw|�g}o©w��|�g}�o��|/g}/o��|�g}�o��|�g}�o��|���w��w}���w|���w!  ��}���w|���w! }���|��x}�|�?> � o& �z�o& ��|��*x}�|�> � o& �|�o& �}/o|/g#}���}�o|�g�����BK^#Vz��ex#y��_x###�Mx#x��Zx#~#fo���}�|�> ?� o& ��}�|�> � o& �     s                               !                                                        A   �����������%3@MZp�����   � � �  � � �   d �       � *      � E    F �  R  � r  � �    � �  � � �  � � �  � 
  � :	( � � T
 d � m      � �      � �   � �     � �     � �     � �     �      � 8     � \     � } 	    � � 
    � �      � �       � �	
   � � 	    �   � � $  � � G  � � r  � � �      � �      � �    � �
    � �    � 	    � $	  � � = 		    � a	   z      � � 	    � �	   � �	   � �	    � 	  � I      � q
   � r
 � � �
  � �
  � �
   � �
  � � �
  � � 	
	  � � 	

  � � 8	
  � � [	
  � � w	      � �	  �  �	  �  �	
 �  �	 
      �	
  �  

   � 9
       K
      � v
-3�/�.�2w
�
�
�
�
0[����9_�����
1R��G���T����1g���+f��������"P��������u��^eu�������0CYk������Xx�� Jg��
6Xw��!H                 �$    �*�*        r    r+  w  w+ a  	a+ 	x  x+                                                                                                                               � ???????????                      �_   �_   �_     �_      �_      �_      �_      �_      �_      �_      �_  �_ �_   �_   �_ b�_�~ �_�~ �_ �_ �_ �_ �_ �_ �_	 �_	     �_�c�eOf
�_�fii           00000                  �@@@@@@@@@@@@@@@@@@@@@@						@@@@@





@@@@       