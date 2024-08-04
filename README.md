# Delphi FMX Game Snippets

[Cette page en français.](LISEZMOI.md)

Examples of what is done when developing video games: sprite management, background music, sound effects, animations, ...

Projects are developed under Delphi with its FireMonkey multiplatform framework to run our projects under Windows, macOS, iOS, Android and Linux from the same code base.

Not all images and musics used in this repository are free of charge. Reuse them only if you have a license. They remain the property of their respective authors and are only present in the programs for demo purposes.

Images and sounds from:

* [Kenney](https://kenney.nl/) which I thank for his work and spirit of sharing. Think of doing the same as me: support him by buying licenses of his creations if you use them even if he does not require us to.
* [The Game Creators](https://www.thegamecreators.com/), software editor for video game creation and games, also offers assets including a noise library used here.
* GSP 500 Noises, a CD-ROM of rights-free noise published in 1995. Yeah, I know, some of you weren't born... There was also a life before the Internet.
* [Cartoon Smart](https://vasur.fr/cartoonsmart) which offers video training in the development of video games for iOS and macOS with the possibility to download free sound and image libraries when you subscribe.

Information and explanations on the projects of this repository are available on [Developeur Pascal blog](https://developpeur-pascal.fr/).

Example projects are in the group fr.developeurpascal.demo.*

Please do not publish them on application stores or elsewhere in a compiled version but systematically [return to this code repository](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets) or to [the dedicated section on the blog Developpeur Pascal](https://developpeur-pascal.fr/delphi-fmx-game-snippets.html).

This library was created [live on Twitch](https://www.twitch.tv/patrickpremartin) during the Gaming For Sidaction operation in favor of [Sidaction 2021](https://sidaction.org). You can view this weekend's replay on https://serialstreameur.fr/sidaction-2021.php

If you want to see the result of the animations in a real video game, take a look at [Ok Ducky](https://okducky.gamolf.fr/) developed live during this live game coding weekend.

Other games were developed throughout the year 2021 and beyond on Twitch. Reruns of these sessions are available on [Serial Streamer Video Games](https://serialstreameur.fr/games-video.php).

If you are looking for examples of code to learn Delphi about something other than video game or basic manipulations see [this repository of examples of all kinds](https://github.com/DeveloppeurPascal/Delphi-samples).And if not, take a look at [these open-source video games on GitHub](https://github.com/topics/delphi-game).

To code your own games in Delphi, there are numerous utilities and game engines. You'll find a few links on [Awesome Pascal](https://github.com/Fr0sT-Brutal/awesome-pascal#game-dev). You can also use FireMonkey natively and add just a few useful tricks with [Delphi Game Engine](https://github.com/DeveloppeurPascal/Delphi-Game-Engine).

This code repository contains some projects developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

## Talks and conferences

### Learn to code Summer Camp 2021

* [Des resources disponibles pour apprendre et des exemples d'animations simples](https://apprendre-delphi.fr/ltcsc2021-04.php) (in French)

### Twitch

Follow my live game development coding sessions on [my Twitch channel](https://www.twitch.tv/patrickpremartin) or as replays on [Serial Streameur](https://serialstreameur.fr/jv-toolbox-delphi.php) mostly in French.

## Using this software

Visit [the devlog](https://developpeur-pascal.fr/delphi-fmx-game-snippets.html) to find out more about how it works, access videos and articles, find out about the different versions available and their features, contact user support...

## Source code installation

To download this code repository, we recommend using "git", but you can also download a ZIP file directly from [its GitHub repository](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets).

This project uses dependencies in the form of sub-modules. They will be absent from the ZIP file. You'll have to download them by hand.

* [DeveloppeurPascal/Delphi-Game-Engine](https://github.com/DeveloppeurPascal/Delphi-Game-Engine) must be installed in the ./lib-externes/Delphi-Game-Engine subfolder.
* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) must be installed in the ./lib-externes/librairies subfolder.

## Compatibility

As an [Embarcadero MVP](https://www.embarcadero.com/resources/partners/mvp-directory), I benefit from the latest versions of [Delphi](https://www.embarcadero.com/products/delphi) and [C++ Builder](https://www.embarcadero.com/products/cbuilder) in [RAD Studio](https://www.embarcadero.com/products/rad-studio) as soon as they are released. I therefore work with these versions.

Normally, my libraries and components should also run on at least the current version of [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

There's no guarantee of compatibility with earlier versions, even though I try to keep my code clean and avoid using too many of the new ways of writing in it (type inference, inline var and multiline strings).

If you detect any anomalies on earlier versions, please don't hesitate to [report them](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets/issues) so that I can test and try to correct or provide a workaround.

## License to use this code repository and its contents

This source code is distributed under the [AGPL 3.0 or later license](https://choosealicense.com/licenses/agpl-3.0/).

You are generally free to use the contents of this code repository anywhere, provided that:
* you mention it in your projects
* distribute the modifications made to the files supplied in this project under the AGPL license (leaving the original copyright notices (author, link to this repository, license) which must be supplemented by your own)
* to distribute the source code of your creations under the AGPL license.

If this license doesn't suit your needs, you can purchase the right to use this project under the [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) or a dedicated commercial license ([contact the author](https://developpeur-pascal.fr/nous-contacter.php) to explain your needs).

These source codes are provided as is, without warranty of any kind.

Certain elements included in this repository may be subject to third-party usage rights (images, sounds, etc.). They are not reusable in your projects unless otherwise stated.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets) and [open a new issue](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

or if you speack french you can [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) on a monthly or yearly basis and get a lot of resources as videos and articles.
