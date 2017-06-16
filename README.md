# Emacs Customized File
This repository consists of two different emacs configuration files.  The basic .emacs file has been customized to my preferences but it allows for future customization if anyone desires to change them.

The second file, .emacs_minimal, is a minified version of my .emacs file designed to work more in favor of using emacs through the terminal with no GUI.

## Package Auto Install
One thing I enjoy most about my emacs file is the ability for it to handle automatically installing.  Usually, a series of commands, such as *M-x list-packages* followed by searching for the package, pressing *i* to mark it for install, then pressing *x* to run the install is required.

Instead, this emacs file aims to allow users to search for a file on a site of their choosing *(I use Melpa very often to find packages to accomplish certain goals for me)* and finding the package there.  

The following are simple instructions for adding new packages to the auto install list.

1. Update the package-list at the top of the .emacs file.
2. Ensure that the package archive that contains the package is in the list *(usually you won't need to do this if you are grabbing from Melpa)*.
3. Set the proper hooks, modes, etc., that are required for the new package.
4. Reload the emacs file *(I do this using M-x load-file ~/.emacs)*

#### Note: If you don't need any different packages from what I need then you don't need to go through the steps above.

## Useful Package Archives
Earlier I mentioned using Melpa to find some packages.  The link is below:

https://melpa.org/#/

This site is great for browing a number of packages to find useful functionality.  However, Melpa isn't compatible with older versions of Emacs.  I currently run 24.x as that is what is available through the Linux package manager and Melpa packages work wonderfully with it.

I will be adding more links as I need them but if there are any suggestions for good packages or package archives please let me know to keep the list up to date.

## Adding New Keys
Because Emacs is great for keeping your hands off the mouse and on the keyboard, it makes sense that you can customize the hotkeys for almost whatever you want.  In the case of these files, I've binded the hotkeys I use most regularly to keys I'm comfortable with.  Obviously this can be a bit of a learning curve for someone if they don't find *C-s* intuitive for searching like I do.  With that in mind, this is the process I usually go through to create custom hotkeys that might be able to offer some insight into making your own:

1. Figure out exactly what command you want to bind to a new hotkey.  If you don't know the emacs commands by heart, this means getting on Google and searching for the command, checking out this greate Emacs Wiki (https://www.emacswiki.org/), or, if you happen to know the current hotkey, you can do a key describe using *C-h k KEY_COMBINATION*.
   
   An example of this is the paste key.  I know that in normal Emacs 24.x, the past key combination is *C-y*.  So when I went to change that a different key, I used *C-h k* which asked me for a key combination.  I hit *C-y* and a description page of the key with the associated command appeared.
   
   This is handy to see what key binding exists that you will be overwriting, assuming there is a key binding, to the new key strokes you will be using for a command (mostly to make sure you aren't getting rid of something important).

2. Once you've got the command you want and decided on a key combination, use the previously defined key bindings as a template for setting global key settings.

   You could also go so far as to set mode specific key bindings but I like my key bindings to work in every mode.
   
This emacs file is customized for what I enjoy using but there is still plenty of room for improvement in the file.  I'm sure there are scenarios I have yet to run into where a package would be really helpful, I just haven't found a need for more at the moment.  If you know of good packages to add that are quality of life packages you use, or mode specific customizations that are helpful, please don't hesitate to let me know.
