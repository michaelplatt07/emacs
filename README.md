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
4. 
