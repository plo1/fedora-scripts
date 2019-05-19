#!/bin/python3

import sys
import os

nvidia = 'GRUB_CMDLINE_LINUX=\"rd.driver.blacklist=nouveau modprobe.blacklist=nouveau nvidia-drm.modeset=1 resume=/dev/mapper/fedora-swap rd.lvm.lv=fedora/root rd.lvm.lv=fedora/swap rhgb quiet\"\n'
intel = 'GRUB_CMDLINE_LINUX=\"resume=/dev/mapper/fedora-swap rd.lvm.lv=fedora/root rd.lvm.lv=fedora/swap rhgb quiet\"\n'
new_grub_path = os.getenv('HOME') + '/grub'


def select(gpu, grub, debug):
    if gpu != 'nvidia' and gpu != 'intel':
        raise Exception('Invalid GPU')

    newcfg = ''
    oldcfg = ''
    grubcfg = intel if gpu == 'intel' else nvidia

    for line in grub:
        if line.find('GRUB_CMDLINE_LINUX=') != -1:
            newcfg += grubcfg
        else:
            newcfg += line
        oldcfg += line

    if debug:
        print('Old Grub\n')
        print(oldcfg)
        print('\n\nNew Grub \n')
        print(newcfg)

    return newcfg, oldcfg


def backup(cfg, debug):
    backup_path = os.getenv('HOME') + '/grub.old'
    backup = open(backup_path, 'w+')
    backup.write(cfg)
    backup.close()


def main():
    if len(sys.argv) == 1:
        raise Exception('Too few args')

    grub = open('/etc/default/grub', 'r+')
    debug = False

    if debug:
        print(sys.argv)
        print('Arg Length: ', len(sys.argv), '\n')

    newcfg, oldcfg = select(sys.argv[1], grub, debug)
    confirm = input('Confirm switch to ' + sys.argv[1] + '?[y/n]: ')

    if confirm == 'y':
        backup(oldcfg, debug)
        grub.seek(0)
        grub.write(newcfg)
        grub.truncate()
        grub.close()
        os.system('sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg')
    else:
        grub.close()


if __name__ == '__main__':
    main()
