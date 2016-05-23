# Iom

A Tool for bit manipulation on media files

The goal of this tool is to provide a way to manipulate bits in a media file in set and predictable ways such that a simple command or mouse click can perform the actions.

It is currently being built with MP3 files in mind and will initially flip the databits so that a section of a song that reads: 00011101011 becomes 11010111000. This will be a subtle flip especially since it is being esigned to leave the header and all other data in tact, but could provide interesting results. 
