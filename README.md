# Archaic

Arch Automatic Installation Commands

(Still somewhat personal)

## What it does

- Main Arch Linux on top of LVM on top of LUKS. Bare minimum desktop with XMonad+xmobar.
- Secondary minimal Arch Linux as a rescue alternative.
- refind boot manager.

## Requirements

- UEFI system.
- Secure Boot disabled from UEFI.
- GPT disk.
- Existing EFI partition (only tested with the one created by Windows 10 in a Lenovo Thinkpad)
- Free un-partitioned disk space ready to be asigned according to instructions. See comments inside [`run_archaic`](run_archaic).
- Live Arch Installation ISO (only tested with a USB stick on March 2018).
- Internet conection.

**IMPORTANT:** There are many more requirements and constraints that you will only understand reading `run_archaic` and `_inside_arch_chroot`.

## Usage

- Consider `rmmod pcspkr` to save your composure.
- Put a copy of this repo inside your **running** Live Arch Installation ISO.
    - You may install `git` with `pacman -Sy` and `pacman -S git`
    - And then `git clone https://github.com/franalbani/archaic`
- Open `run_archaic` in a text editor, read it, check it, double-check it and edit the required variables. Left `$debug=` commented, so you can do a dry-run.
    - Remember to choose a decent hostname. Some tips:
        - don't use your name like "franpc" or "frannotebook" because then your prompt will be silly like "fran@franpc".
        - don't use a generic name like "pc" or "notebook" because you may have more than one in the future.
        - You don't name your dog "dog" or "frandog" or "mydog".
        - Take the chance to honor some person, place or concept.
        - Favor shorter words, to avoid having to type too much when login remotely.
- Run `run_archaic | tee /tmp/archaic.log` script. It should be in **debug** mode so you can check that all text replacements went fine.
- Re-open `run_archaic` and uncomment the `debug=` line.
- Edit `xorg_pkgs` to reflect your video driver.
- Run `run_archaic | tee /tmp/archaic.log` script and wait for input prompts.
- Do not relax too much.

If you need to modify the code, remember: [shellcheck](https://github.com/koalaman/shellcheck) is your friend
(but ignore SC2086 recommendation for `$PKGS`).


## In case of errors

To restart the process, remember to:
- delete `/boot/EFI/refind/`
- delete all kernels and initramfs in `/boot/`

Full restart from initial conditions is recommended, but if time needs to be saved, careful commenting of sections can be made in scripts (can be very challenging).

## Inspiration

- https://shirotech.com/linux/how-to-automate-arch-linux-installation

## Wanna help?

- Better diagnose of errors.
- Smarter pacman repository to avoid redownloading packages.
- Remove lot of duplicate and ugly bash code.
- Ease testing with virtualization.
- Suspend to RAM vs. crypt. What to do?
- Many steps should be optional: backups, secondary_arch, etc.
- Many hardcoded options: keyboard layout, timedate locale, etc.
