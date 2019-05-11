#!/bin/python3

import sys
import os

nvidia = 'GRUB_CMDLINE_LINUX=\"rd.driver.blacklist=nouveau modprobe.blacklist=nouveau nvidia-drm.modeset=1 resume=/dev/mapper/fedora-swap rd.lvm.lv=fedora/root rd.lvm.lv=fedora/swap rhgb quiet\"\n'
intel = 'GRUB_CMDLINE_LINUX=\"resume=/dev/mapper/fedora-swap rd.lvm.lv=fedora/root rd.lvm.lv=fedora/swap rhgb quiet\"\n'
new_grub_path = os.getenv('HOME') + '/grub'


def select(gpu, old_grub, new_grub, debug):
    if gpu != 'nvidia' and gpu != 'intel':
        print('Invalid GPU')

    temp = ''
    oldcfg = ''
    grubcfg = intel if gpu == 'intel' else nvidia

    for line in old_grub:
        if line.find('GRUB_CMDLINE_LINUX=') != -1:
            temp += grubcfg
        else:
            temp += line
        oldcfg += line

    if debug:
        print('Old Grub\n')
        print(oldcfg)
        print('\n\nNew Grub \n')
        print(temp)

    new_grub.write(temp)


def main():
    if len(sys.argv) == 1:
        print('Too few args')
        return

    old_grub = open('/etc/default/grub', 'r')
    new_grub = open(new_grub_path, 'w+')
    debug = False

    if debug:
        print(sys.argv)
        print('Arg Length: ', len(sys.argv), '\n')

    select(sys.argv[1], old_grub, new_grub, debug)
    old_grub.close()
    new_grub.close()

    confirm = input('Confirm switch to ' + sys.argv[1] + '?[y/n]: ')
    if confirm == 'y':
        os.system('sudo mv ' + new_grub_path + ' /etc/default/grub')
        os.system('sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg')


if __name__ == '__main__':
    main()
