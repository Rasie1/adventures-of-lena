# Adventures of Lena

This is a platformer game written for my girlfriend's birthday using Haskell and SDL2.
Upon completing each level, score was sent to my server. After that, I sent the score to her bank account :)


# Install

For running this game you will need `libsdl2-dev libsdl2-image-dev libsdl2-mixer-dev` packages. On ubuntu, you can install it this way:
```
sudo apt update
sudo apt install libsdl2-dev libsdl2-image-dev libsdl2-mixer-dev
```

In case they are not found, you may have no universe repository added. Add it with:
```
sudo add-apt-repository universe
sudo apt update
```
Then try to install packages again

# Run

`./game`
