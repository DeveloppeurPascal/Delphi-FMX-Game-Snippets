# Liste des projets

Les projets de ce dépôt sont :

* AnimationDroiteGauche

Création d'une image composée de plusieurs qui va faire des aller/retours en bas de l'écran.

* AnimationSprite

Transformation de l'image précédente en "sprite" géré sous forme de TFrame indépendant permettant de le réutiliser.

* AnimationPlusieursCanardsALEcran

Utilisation du sprite du canard et multiplication de celui-ci à l'écran sur plusieurs niveaux.

* MusiqueDAmbiance

Création d'un librairie pour jouer des musiques en fond sonore des logiciels. Elle est réutilisable en l'état.
Le composant TMediaPlayer de FireMonkey est utilisé. Une évolution plus pratique pour améliorer les choses serait de passer directement par les différentes API de sons/musiques des systèmes d'exploitation ciblés.

* EffetsSonores

Utilisation de la librairie musicale et adaptation pour jouer des son uniques ou à plusieurs.

* AnimationVagues

Dessin et animation de vagues sur plusieurs niveaux. C'est la base des animations en parallax (plusieurs niveaux de vitesse différente).

* AnimationVaguesEtCanards

Maintenant que vous savez faire des vagues, nous ajoutons des canards sur les différents niveaux.

* AnimationGrossissement

Une animation classique dans les jeux vidéos : on touche quelque chose, il grossit jusqu'à traverser l'écran vers nous et disparaît.

* CanardPivot

On continue avec ce pauvre canard qui va maintenant pouvoir réagir si on clique sur lui ou son socle.

* CreateSpriteSheet

Projet déplacé vers [un dépôt dédié](https://github.com/DeveloppeurPascal/Spritesheet-Creator).

* AnimSpriteExplosion

Utilisation d'une spritesheet pour montrer comment fonctionne le TBitmapAnimation.

* AnimSpriteEauDuLac

On en profite pour faire de l'eau qui bouge sur la rive d'un lac. L'image utilisée n'étant pas top, l'effet est bizarre mais le principe reste le même avec de meilleures illustrations.

* AnimSpritePersonnage

Et on finit par faire marcher puis sauter un ninja...

* DuckJoke

Tout est dans le nom... Un programme créé un 1er avril que je dédie à toutes les personnes qui oublient de verrouiller leur session lorsqu'elles sont dans les transports en commun.

Téléchargez le programme compilé depuis https://gamolf.itch.io/duck-joke

* JeuDesCercles

Un exemple de programme avec des cercles en guise de sprite qui permet de gérer leurs mouvements, les collisions entre eux et rebonds.

* AnimGagneDesPoints

Animation lorsqu'on gagne des points dans un jeu.

* AnimScore

Animation du changement de valeur d'un score.

* AffichageTexteGeneriqueStarWars

Affichage d'un texte façon générique de Star Wars (défilement du bas vers le centre de l'écran en profondeur).

Image de fond provenant de la [NASA](https://github.com/DeveloppeurPascal/DelphiFMXGameSnippets/tree/main/AffichageTexteGeneriqueStarWars/NASA/README.md).
Son d'ambiance provenant de https://www.soundboard.com/sb/sound/918028
(téléchargé au premier lancement du programme dans le dossier de l'exe)

Utilise une unité du projet https://github.com/DeveloppeurPascal/librairies à placer dans la même arborescence que ce groupe de projet. "librairies" doit être au niveau de "DelphiFMXGameSnippets".

* AnimMeteoNeige

Effet de neige qui tombe sur plusieurs avec éventuelement du vent.

* AnimCroixQuiTournent

Le but du jeu était de refaire l'animation correspondant à ce GIF:
https://twitter.com/OrgPhysics/status/1200324633096065024
Des croix blanches et noires qui tournent en alternance.

Cet exemple utilise les composants Radiant Shapes disponibles depuis GetIt.
https://getitnow.embarcadero.com/bonus-radiant-shapes/

* MaleCharacterSVGWalkingAnimation

Exemple d'animation basées sur les SVG d'un personnage qui marche provenant du [Toon Characters 1](https://www.kenney.nl/assets/toon-characters-1) pack de [Kenney](https://www.kenney.nl/) en utilisant la librairie Skia pour la conversion SVG vers TBitmap.

* Match-3-Game-Sample

Un exemple de jeu de type match-3 utilisant des éléments de Kenney.nl en SVG pour la partie graphique. Ce projet peut servir de base aux vôtres : les traitement sont dans une TFrame et prennent en charge les orientations ou tailles de fenêtres comme la résolution d'écran (BitmapScale). Il fonctionne sur toutes les plateformes prises en charge par FireMonkey.

* MusicLoopUnitTest

Programme multiplateforme de test des sons et musiques joués par l'unité uMusicLoop.pas
(utile pour s'assurer de l'absence de bogue sur les plateformes à chaque version de Delphi)

Utilise une unité du projet https://github.com/DeveloppeurPascal/librairies à placer dans la même arborescence que ce groupe de projet. "librairies" doit être au niveau de "DelphiFMXGameSnippets".

Le son de test (WAV) est téléchargé au lancement du programme et mis en cache depuis la page https://opengameart.org/content/win-sound-effect avec comme url https://opengameart.org/sites/default/files/Win%20sound.wav

La musique de test (MP3) est téléchargée au lancement du programme et mis en cache depuis la page https://opengameart.org/content/battle-theme-b-for-rpg avec comme URL https://opengameart.org/sites/default/files/battleThemeB.mp3

* UtilisationSpriteSheet

Exemple d'utilisation et de découpage d'une spritesheet formatée en blocs.

Sprite sheet utilisée comme démo provenant de https://kenney.nl/assets/pixel-platformer

* SpriteSheetSplitter

Projet déplacé vers [un dépôt dédié](https://github.com/DeveloppeurPascal/Spritesheet-Splitter).

* BitmapHorizReverse

Fait une symétrie horizontale pour retourner une image et en crée une autre (utile pour récupérer des bas de zone de décor à partir des images du haut).

cf sprites utilisés dans Egg Hunter (vidéos de création sur https://serialstreameur.fr/jv-egg-hunter.php, jeu à télécharger sur https://gamolf.itch.io/egg-hunter)

* AnimRotationImages

Deux animations à ne pas conseiller aux épileptiques mais que les amateurs de [Alfred Hitchcock](https://fr.wikipedia.org/wiki/Alfred_Hitchcock) devraient apprécier.

* StarField

Un champ d'étoile qui bouge. Déplacement avec flèches du clavier.
