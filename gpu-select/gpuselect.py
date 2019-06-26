#!/bin/python3

import sys
import os

'''Make Sure to copy this file to /usr/local/bin'''

nvidia = 'rd.driver.blacklist=nouveau modprobe.blacklist=nouveau ' \
    'nvidia-drm.modeset=1 '
backup_path = os.getenv('HOME') + '/grub.old'
grub_path = '/etc/default/grub'


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
    cmdline_linux = 'GRUB_CMDLINE_LINUX='

    for line in grub.splitlines():
        if line.find(cmdline_linux) is not -1:
            newcfg += cmdline_linux + edit(gpu, line[len(cmdline_linux):]) \
                + '\n'
        else:
            newcfg += line + '\n'

    return newcfg


def backup():
    with open(grub_path, 'r') as f1, open(backup_path, 'w+') as f2:
        f2.write(f1.read())


def main():
    if len(sys.argv) <= 1:
        raise Exception('Too few args')

    confirm = input('Confirm switch to ' + sys.argv[1] + '? [y/n]: ')
    if confirm == 'y':
        with open(grub_path, 'r+') as grub:
            temp = grub.read()
            newcfg = select(sys.argv[1], temp)
            backup()
            grub.seek(0)
            grub.write(newcfg)
            grub.truncate()
            os.system('sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg')


if __name__ == '__main__':
    main()
