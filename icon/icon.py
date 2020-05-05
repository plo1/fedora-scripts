#!/bin/python3

import os
import sys

home = os.getenv('HOME')
icons = {}
icon_dir = home + '/.local/share/applications/'

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

matlab = '[Desktop Entry]\n' \
    'Name=Matlab\n' \
    'Type=Application\n' \
    'Exec=' + '\"/usr/local/MATLAB/R2020a/bin/matlab\" -desktop -r \"startup(%f)\"\n' \
    'Icon=' + '/usr/local/MATLAB/R2020a/bin/glnxa64/cef_resources/matlab_icon.png\n' \
    'Terminal=False\n' \
    'Comment=Computational Programming Environment\n' \
    'Version=R2020a\n' \
    'Categories=Development;IDE;\n'

matlab_nvidia = '[Desktop Entry]\n' \
    'Name=Matlab(NVIDIA)\n' \
    'Type=Application\n' \
    'Exec=env __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia ' \
        + '\"/usr/local/MATLAB/R2020a/bin/matlab\" -desktop -nosoftwareopengl -r \"startup(%f)\"\n' \
    'Icon=' + '/usr/local/MATLAB/R2020a/bin/glnxa64/cef_resources/matlab_icon.png\n' \
    'Terminal=False\n' \
    'Comment=Computational Programming Environment\n' \
    'Version=R2020a\n' \
    'Categories=Development;IDE;\n' \


def init_icons():
    icons['eclipse.desktop'] = eclipse
    icons['android-studio.desktop'] = android_studio 
    icons['matlab.desktop'] = matlab
    icons['matlab_NVIDIA.desktop'] = matlab_nvidia


def create_icon_single():
    for key, value in icons.items():
        if input(value + '\nCreate ' + key + '? [y/n]: ') == 'y':
            with open(icon_dir + key , 'w+') as f:
                f.write(value)


def create_icon_batch():
    for key, value in icons.items():
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
        init_icons()
    else:
        return

    if len(sys.argv) == 2 and sys.argv[1] == '-a':
        create_icon_batch()
    else:
        create_icon_single()


if __name__ == '__main__':
    main()
