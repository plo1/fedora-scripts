#!/bin/python

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
        return line[:1] + nvidia + line[1:]
    else:
        return line


def select(gpu, path):
    if gpu != 'nvidia' and gpu != 'intel':
        raise Exception('Invalid GPU')

    newcfg = ''
    oldcfg = ''
    prefix = 'GRUB_CMDLINE_LINUX='

    with open(path, 'r') as f:
        for line in f:
            if line.find(prefix) is not -1:
                newcfg += prefix + edit(gpu, line[len(prefix):])
            else:
                newcfg += line
            oldcfg += line

    return newcfg, oldcfg


def write(cfg):
    with open(grub_path, 'w') as f:
        f.write(cfg)


def backup(cfg):
    with open(backup_path, 'w+') as f:
        f.write(cfg)


def main():
    if len(sys.argv) <= 1:
        raise Exception('Too few args')

    confirm = input('Confirm switch to ' + sys.argv[1] + '? [y/n]: ')
    if confirm == 'y':
        newcfg, oldcfg = select(sys.argv[1], grub_path)
        backup(oldcfg)
        write(newcfg)
        os.system('sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg')


if __name__ == '__main__':
    main()
