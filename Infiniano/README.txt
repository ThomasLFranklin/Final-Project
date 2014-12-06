Infiniano(R): The Infinite Piano

      by N. Dara, T. Franklin, D. Grove, and B. Weeks.

  WWW: http://github.com/ThomasLFranklin/Final-Project/

The documentation for Infiniano is licensed under the Creative Commons
Attribution 3.0 license:
http://creativecommons.org/licenses/by/3.0/legalcode .

"Infiniano" is a registered trademark of Do It For The Rackets Group(TM).

Version 1.0

Contents of this README:

1.  Licensing
2.  Instructions
3.  Known Issues/Intended fixes
4.  Source Code

--------------------------------------------------------------------------------

1. Licensing

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version. The program source code is also freely
available as per Section 4 of this README.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

-------------------------------------------------------------------------------

2. Instructions 

 * To run the program, select AUTORUN.exe
 * After a loading delay, you will be brought to a startup screen. Click to
   continue.
 * You will next be shown the Terms and Conditions of Infiniano. When you are
   done viewing them, click anywhere in the window to continue. This
   constitutes your agreement with these terms.
   
 MAIN MENU:
 Freeplay - click the "Freeplay" button to be brought to the main, unrestricted
            piano screen.
          - Use the keys of your keyboard to play the corresponding piano key
            tone on screen.
          - To adjust which two octaves you are playing, use the arrow keys to
            toggle going up or down two octaves on the keyboard.
          - To adjust program volume, use the slider located at the
            middle-bottom of the screen.
          - To switch instruments, select the check box adjacent to the
            corresponding instrument to switch the tones of the keys to
            tones mimicing that instrument.
 Demo Songs - Select the "Demo Songs" button to choose from a list of six
              pre-recorded MIDI songs. Select the corresponding song with your
              mouse to hear it play.
            - To stop the current song from playing, select the red "stop
              button" in the middle of the window.
 Instructions - Selecting this "Instructions" button will bring you to a
                placeholder page. This is intended to graphically display the
                information available in this section of the README, but has
                been excluded in version 1.0 to avoid redundancy.

-------------------------------------------------------------------------------

3. Known Issues in 1.0 & Intended Fixes

Lag is a well-known issue of this program. As we are all relatively
novice programmers, some of our code has not entirely been optimized for
performance. However, the code should work as intended, as this program was
developed with process and function in mind, with not as much emphasis on
performance.

 * Keys will not hold notes when held down unless the default Key Repeat
   option is turned off in Windows. See "the Windows Configuration GUide.pdf"
   for instructions on how to disable this for improved performance.
 
We are oprimismistic that we may add in the future:
 * Support for our program to include a recording feature.
 * A metronome to assist with this.


-------------------------------------------------------------------------------

4.  Source Code and Other Information

Source code to this program is always available; for more information visit
our web site at:

  http://github.com/ThomasLFranklin/Final-Project/
  
Infinano is written entirely in Racket and was coded using the DrRacket IDE, a
free to use program avalable at:

  http://www.racket-lang.org/download/

--------------------------------------------------------------------------------
Additional copyright information:
--------------------------------------------------------------------------------

Do It For The Rackets(TM)

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
