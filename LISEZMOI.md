# Delphi FMX Game Snippets

[This page in English.](README.md)

> [!WARNING]
> Suite à la décision de Microsoft de fondre GitHub dans sa division IA à l'été 2025, j'ai pris la décision d'arrêter la maintenance des dépôts ici. Je mettrai à jour ce dépôt de temps en temps par copie des modifications manquantes depuis le dépôt principal désormais sur [Codeberg](https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets).

Des exemples de ce qui se fait lorsqu'on développe des jeux vidéos : gestion de sprites, musiques de fond, effets sonores, animations, ...

Les projets sont développés sous Delphi avec son framework multi plateformes FireMonkey pour exécuter nos projets sous Windows, macOS, iOS, Android et Linux à partir de la même base de code.

Les images et musiques utilisées dans ce dépôt ne sont pas toutes libres de droit. Ne les réutilisez que si vous en avez une licence. Ils restent la propriété de leurs auteurs respectifs et ne sont présents dans les programmes qu'à des fins de démo.

Sont utilisées des images et des sons provenant de :

* [Kenney](https://kenney.nl/) que je remercie pour son oeuvre et son esprit de partage. Pensez à faire comme moi : soutenez le en achetant des licences de ses créations si vous les utilisez même s'il ne nous y oblige pas.
* [The Game Creators](https://www.thegamecreators.com/), éditeur de logiciels de création de jeux vidéos et de jeux, propose aussi des assets dont une librairie de bruitages utilisée ici.
* GSP 500 Noises, un CD-Rom de bruitages libres de droits publié en 1995. Oui, je sais, certains d'entre vous n'étaient pas nés... Il y avait aussi une vie avant l'Internet.
* [Cartoon Smart](https://vasur.fr/cartoonsmart) qui propose des vidéos de formation au développement de jeux vidéos pour iOS et macOS avec la possibilité de télécharger des librairies de sons et d'images libres de droit lorsqu'on est abonné.

Des informations et explications sur les projets de ce dépôt sont disponibles sur [le blog Développeur Pascal](https://developpeur-pascal.fr/delphi-fmx-game-snippets.html).

Les projets d'exemple sont dans le groupe fr.developpeurpascal.demo.*

Merci de ne pas les diffuser sur les magasins d'application ni ailleurs en version compilée mais de systématiquement [renvoyer vers ce dépôt de code](https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets) ou vers [la rubrique dédiée sur le blog Développeur Pascal](https://developpeur-pascal.fr/delphi-fmx-game-snippets.html).

Cette librairie a été créée [en direct sur Twitch](https://www.twitch.tv/patrickpremartin) lors de l'opération Gaming For Sidaction en faveur du [Sidaction 2021](https://sidaction.org). Vous pouvez consulter le replay de ce week-end sur https://serialstreameur.fr/sidaction-2021.php

Si vous voulez voir le résultat des animations dans un vrai jeu vidéo, jetez donc un oeil à [Ok Ducky](https://okducky.gamolf.fr/) développé en direct lors de ce week-end de game coding en live.

D'autres jeux ont été développés tout au long de l'année 2021 et au delà sur Twitch. Les rediffusions de ces sessions sont disponibles sur [la rubrique Jeux Vidéo de Serial Streameur](https://serialstreameur.fr/jeux-video.php).

Si vous cherchez des exemples de code pour apprendre Delphi sur autre chose que du jeu vidéo ou des manipulations de base consultez [ce dépôt d'exemples de toutes sortes](https://codeberg.org/PatrickPremartin/Delphi-samples). Et sinon jetez un coup d'oeil à [ces jeux vidéo au code source ouvert sur GitHub](https://github.com/topics/delphi-game).

Pour coder vos propres jeux en Delphi il existe de nombreux utilitaires et moteurs de jeux vidéo. Vous trouverez quelques liens sur [Awesome Pascal](https://github.com/Fr0sT-Brutal/awesome-pascal#game-dev). Vous pouvez aussi utiliser FireMonkey nativement et ajouter juste quelques trucs utiles grâce à [Delphi Game Engine](https://github.com/DeveloppeurPascal/Delphi-Game-Engine) ou passer par [ce kit de démarrage](https://github.com/DeveloppeurPascal/Gamolf-FMX-Game-Starter-Kit).

Ce dépôt de code contient des projets développés en langage Pascal Objet sous Delphi. Vous ne savez pas ce qu'est Dephi ni où le télécharger ? Vous en saurez plus [sur ce site web](https://delphi-resources.developpeur-pascal.fr/).

## Présentations et conférences

### Learn to code Summer Camp 2021

* [Des resources disponibles pour apprendre et des exemples d'animations simples](https://apprendre-delphi.fr/ltcsc2021-04.php) (in French)

### Twitch

Suivez mes streams de développement de logiciels, jeux vidéo, applications mobiles et sites web sur [ma chaîne Twitch](https://www.twitch.tv/patrickpremartin) ou en rediffusion sur [Serial Streameur](https://serialstreameur.fr) la plupart du temps en français.

## Installation des codes sources

Pour télécharger ce dépôt de code il est recommandé de passer par "git" mais vous pouvez aussi télécharger un ZIP directement depuis [son dépôt Codeberg](https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets).

Ce projet utilise des dépendances sous forme de sous modules. Ils seront absents du fichier ZIP. Vous devrez les télécharger à la main.

* [DeveloppeurPascal/Delphi-Game-Engine](https://github.com/DeveloppeurPascal/Delphi-Game-Engine) doit être installé dans le sous dossier ./lib-externes/Delphi-Game-Engine
* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) doit être installé dans le sous dossier ./lib-externes/librairies

## Documentation et assistance

Je passe par des commentaires au format [XMLDOC](https://docwiki.embarcadero.com/RADStudio/fr/Commentaires_de_documentation_XML) dans Delphi pour documenter mes projets. Ils sont reconnus par Help Insight qui propose de l'aide à la saisie en temps réel dans l'éditeur de code.

J'utilise régulièrement l'outil [DocInsight](https://devjetsoftware.com/products/documentation-insight/) pour les saisir et contrôler leur formatage.

L'export de la documentation est fait en HTML par [DocInsight](https://devjetsoftware.com/products/documentation-insight/) ou [PasDoc](https://pasdoc.github.io) vers le dossier /docs du dépôt. Vous y avez aussi [accès en ligne](https://developpeurpascal.github.io/Delphi-FMX-Game-Snippets) grâce à l'hébergement offert par GitHub Pages.

D'autres informations (tutoriels, articles, vidéos, FAQ, présentations et liens) sont disponibles sur [le site web du projet](https://fmxgamesnippets.developpeur-pascal.fr) ou [le devlog du projet](https://developpeur-pascal.fr/delphi-fmx-game-snippets.html).

Si vous avez besoin d'explications ou d'aide pour comprendre ou utiliser certaines parties de ce projet dans le vôtre, n'hésitez pas à [me contacter](https://developpeur-pascal.fr/nous-contacter.php). Je pourrai soit vous orienter vers une ressource en ligne, soit vous proposer une assistance sous forme de prestation payante ou gratuite selon les cas. Vous pouvez aussi me faire signe à l'occasion d'une conférence ou pendant une présentation en ligne.

## Compatibilité

En tant que [MVP Embarcadero](https://www.embarcadero.com/resources/partners/mvp-directory) je bénéficie dès qu'elles sortent des dernières versions de [Delphi](https://www.embarcadero.com/products/delphi) et [C++ Builder](https://www.embarcadero.com/products/cbuilder) dans [RAD Studio](https://www.embarcadero.com/products/rad-studio). C'est donc dans ces versions que je travaille.

Normalement mes librairies et composants doivent aussi fonctionner au moins sur la version en cours de [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

Aucune garantie de compatibilité avec des versions antérieures n'est fournie même si je m'efforce de faire du code propre et ne pas trop utiliser les nouvelles façons d'écrire dedans (type inference, inline var et multilines strings).

Si vous détectez des anomalies sur des versions antérieures n'hésitez pas à [les rapporter](https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets/issues) pour que je teste et tente de corriger ou fournir un contournement.

## Licence d'utilisation de ce dépôt de code et de son contenu

Ces codes sources sont distribués sous licence [AGPL 3.0 ou ultérieure](https://choosealicense.com/licenses/agpl-3.0/).

Vous êtes libre d'utiliser le contenu de ce dépôt de code n'importe où à condition :
* d'en faire mention dans vos projets
* de diffuser les modifications apportées aux fichiers fournis dans ce projet sous licence AGPL (en y laissant les mentions de copyright d'origine (auteur, lien vers ce dépôt, licence) obligatoirement complétées par les vôtres)
* de diffuser les codes sources de vos créations sous licence AGPL

Si cette licence ne convient pas à vos besoins (notamment pour un projet commercial) je propose aussi [des licences classiques pour les développeurs et les entreprises](https://fmxgamesnippets.developpeur-pascal.fr).

Certains éléments inclus dans ce dépôt peuvent dépendre de droits d'utilisation de tiers (images, sons, ...). Ils ne sont pas réutilisables dans vos projets sauf mention contraire.

Les codes sources de ce dépôt de code comme leur éventuelle version compilée sont fournis en l'état sans garantie d'aucune sorte.

## Comment demander une nouvelle fonctionnalité, signaler un bogue ou une faille de sécurité ?

Si vous voulez une réponse du propriétaire de ce dépôt la meilleure façon de procéder pour demander une nouvelle fonctionnalité ou signaler une anomalie est d'aller sur [le dépôt de code sur Codeberg](https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets) et [d'ouvrir un ticket](https://codeberg.org/PatrickPremartin/Delphi-FMX-Game-Snippets/issues).

Si vous avez trouvé une faille de sécurité n'en parlez pas en public avant qu'un correctif n'ait été déployé ou soit disponible. [Contactez l'auteur du dépôt en privé](https://developpeur-pascal.fr/nous-contacter.php) pour expliquer votre trouvaille.

Vous pouvez aussi cloner ce dépôt de code et participer à ses évolutions en soumettant vos modifications si vous le désirez. Lisez les explications dans le fichier [CONTRIBUTING.md](CONTRIBUTING.md).

## Soutenez ce projet et son auteur

Si vous trouvez ce dépôt de code utile et voulez le montrer, merci de faire une donation [à son auteur](https://github.com/DeveloppeurPascal). Ca aidera à maintenir ce projet et tous les autres.

Vous pouvez utiliser l'un de ces services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [en français](https://ko-fi.com/patrick_premartin_fr) ou [en anglais](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)

Vous pouvez acheter une licence d'utilisateur pour [mes logiciels](https://lic.olfsoftware.fr/products.php?lng=fr) et [mes jeux vidéo](https://lic.gamolf.fr/products.php?lng=fr) ou [une licence de développeur pour mes bibliothèques](https://lic.developpeur-pascal.fr/products.php?lng=fr) si vous les utilisez dans vos projets.

Je suis également disponible en tant que prestataire pour vous aider à utiliser ce projet ou d'autres, comme pour vos développements de logiciels, applications mobiles et sites Internet. [Contactez-moi](https://vasur.fr/about) pour en discuter.
