## Installing NixOS

```
$ fdisk --list
$ fdisk /dev/sda1 (see separate instructions for details)
$ mkfs.ext4 -L nixos /dev/sda1
$ mkswap -L swap /dev/sda2
$ swapon /dev/sda2
$ mount /dev/disk/by-label/nixos /mnt
$ nixos-generate-config --root /mnt
$ nix-env -i wget
$ cd /mnt/etc/nixos
$ wget https://raw.githubusercontent.com/pederpus/nixos-config/master/configuration.nix 
$ nixos-install
$ reboot # Remember to remove the install disk!!
```

### Setting up partitions with fdisk
1. `fdisk --list` too see device name (e.g. /dev/sda)
2. `fdisk <device-name>`
3. `n` to create new partition
4. `p` for primary partiton
5. Leave Partition number at default value (1)
6. Leave First sector at default value
7. +32GiB (or whatever size you want) for last sector.
8. Repeat step 3 - 6 in order to create swap partition
9. Leave Last sector at default value
10. Press `a` and make partition 1 bootable
11. Press `t` and make partition 2 of type 82 (Linux swap)
12. Verify by printing partitions with `p`
13. Save and exit by pressing `w`

### TODO
- Toggle gaps
- Terminal scratchpad
- Style rofi launcher
