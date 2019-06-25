#!/bin/python3

import sys
import os

nvidia = 'rd.driver.blacklist=nouveau modprobe.blacklist=nouveau nvidia-drm.modeset=1'
new_grub_path = os.getenv('HOME') + '/grub'


def edit(gpu, line):
    if gpu == 'intel':
        return line.replace(nvidia, '')

    # gpu == 'nvidia'
    if line.find(nvidia) is -1:
        return '\"' + nvidia + line[1:]
    else:
        return line


def select(gpu, grub):
    if gpu != 'nvidia' and gpu != 'intel':
        raise Exception('Invalid GPU')

    newcfg = ''
    oldcfg = ''
    cmdline_linux = 'GRUB_CMDLINE_LINUX='

    for line in grub:
        if line.find(cmdline_linux) is not -1:
            newcfg += cmdline_linux + edit(gpu, line[len(cmdline_linux):])
        else:
            newcfg += line
        oldcfg += line

    return newcfg, oldcfg


def backup(cfg):
    backup_path = os.getenv('HOME') + '/grub.old'
    backup_file = open(backup_path, 'w+')
    backup_file.write(cfg)
    backup_file.close()


def main():
    if len(sys.argv) == 1:
        raise Exception('Too few args')

    grub = open('/etc/default/grub', 'r+')
    debug = False

    if debug:
        print(sys.argv)
        print('Arg Length: ', len(sys.argv), '\n')

    newcfg, oldcfg = select(sys.argv[1], grub, debug)
    confirm = input('Confirm switch to ' + sys.argv[1] + '? [y/n]: ')

    if confirm == 'y':
        backup(oldcfg)
        grub.seek(0)
        grub.write(newcfg)
        grub.truncate()
        grub.close()
        os.system('sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg')
    else:
        grub.close()


if __name__ == '__main__':
    main()
