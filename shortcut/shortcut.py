#!/bin/python3

import os
import sys

home = os.getenv('HOME')
shortcuts = {}
icon_dir = home + '/.local/share/applications/'

def init_shortcut():
    eclipse = '[Desktop Entry]\n' \
        'Name=Eclipse\n' \
        'Type=Application\n' \
        'Exec=' + home + '/eclipse/java-latest-released/eclipse/eclipse\n' \
        'Icon=' + home + '/eclipse/java-latest-released/eclipse/icon.xpm\n' \
        'Terminal=false\n' \
        'Comment=Integrated Development Environment\n' \
        'NoDisplay=false\n' \
        'Categories=Development;IDE;\n' \
        'Name[en]=Eclipse\n'

    android_studio = '[Desktop Entry]\n' \
        'Type=Application\n' \
        'Name=Android Studio\n' \
        'Icon=' + home + '/android-studio/bin/studio.png\n' \
        'Exec=' + home + '/android-studio/bin/studio.sh\n' \
        'Terminal=false\n' \
        'Categories=Development;IDE;\n'

    shortcuts['eclipse.desktop'] = eclipse
    shortcuts['android-studio.desktop'] = android_studio 

    
def create_shortcut_single():
    for key, value in shortcuts.items():
        if input(value + '\nCreate icon ' + key + ' ? [y/n]: ') == 'y':
            with open(icon_dir + key , 'w+') as f:
                f.write(value)


def create_shortcut_batch():
    for key, value in shortcuts.items():
        with open(icon_dir + key, 'w+') as f:
            f.write(value)

def main():
    if os.geteuid() == 0:
        raise Exception('User is ROOT')
    if not os.path.isdir(icon_dir):
        raise Exception('Icon directory does not exist')
    if len(sys.argv) > 2:
        raise Exception('Too many args')

    if input('Icon directory:\n\t' + icon_dir + '\nIs this correct? [y/n]: ') == 'y':
        init_shortcut()
    else:
        return

    if len(sys.argv) == 2 and sys.argv[1] == '-a':
        create_shortcut_batch()
    else:
        create_shortcut_single()
        
    
if __name__ == '__main__':
    main()
