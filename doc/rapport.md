# Arbre Généalogique

## I - Résumé

### 1. Objectifs

Le projet consiste à développer une application permettant de gérer un arbre généalogique. L’objectif est de modéliser les relations familiales à travers des structures de données adaptées et de permettre l’interaction avec l’arbre (ajout, suppression, modification des individus et des relations).

### 2. Contenu

Ce rapport décrit l’architecture de l’application, les choix techniques réalisés, les algorithmes et types de données utilisés, ainsi que la démarche adoptée pour tester le programme. Il présente également les difficultés rencontrées, les solutions adoptées et les perspectives d’évolution du projet.

## II - Introduction

### 1. Problématique

La gestion d’un arbre généalogique peut devenir rapidement complexe en raison du nombre d’individus et des relations à gérer. Ce projet vise à proposer une solution claire et efficace pour modéliser un arbre généalogique, en garantissant la possibilité d'ajouter, de supprimer et de rechercher des membres, tout en maintenant une structure de données optimisée.

### 2. Plan

Ce document est structuré comme suit :
- L’introduction présente la problématique à laquelle répond le projet.
- L'architecture de l’application et les modules qui la composent sont détaillés.
- Les choix réalisés lors du développement sont expliqués.
- Les algorithmes et types de données utilisés sont présentés.
- Les tests effectués sur l’application sont décrits.
- Les difficultés rencontrées et les solutions adoptées sont abordées.
- Enfin, un bilan technique et personnel est effectué.

## III - Architecture de l'application

L'application a été développée en Ada et se compose des modules suivants :
1. **Module de gestion des individus** : Ce module permet d’ajouter, de supprimer et de modifier les individus dans l’arbre généalogique.
2. **Module de gestion des relations** : Il gère la création et la suppression des relations entre les individus.
3. **Module de représentation graphique** : Bien que l’application soit réalisée en Ada, des outils externes peuvent être intégrés pour la visualisation, si nécessaire.
4. **Module de gestion des données** : Il s’occupe de l’importation, de l’exportation et de la persistance des données.
5. **Module de recherche** : Ce module permet de rechercher des individus par différents critères.

## IV - Principaux choix réalisés

Les principaux choix ont concerné :
- **Langage de programmation** : Le projet a été entièrement réalisé en Ada, en raison de sa robustesse et de sa gestion efficace de la mémoire, ce qui est particulièrement adapté pour ce genre d'application.
- **Structure des données** : Les individus et leurs relations sont modélisés à l'aide de types définis par l'utilisateur, qui permettent une gestion flexible des membres de l'arbre généalogique.
- **Modularité** : L’application est construite autour de modules indépendants permettant de séparer les préoccupations (gestion des individus, des relations, etc.).

## V - Algorithmes et types de données

### Algorithmes

- **Ajout d’un individu** : L’algorithme d'ajout vérifie d'abord l'existence des parents avant d’ajouter l’individu et de lier les relations correspondantes.
- **Suppression d’un individu** : Lors de la suppression d’un individu, toutes les relations qui le concernent sont également supprimées, et l’arbre est mis à jour.
- **Recherche** : La recherche d’un individu se fait à l’aide d’une recherche linéaire, utilisant un identifiant unique pour chaque individu.

### Types de données

Les types principaux utilisés sont :
- **Individu** : Un type représentant un individu, avec des champs pour le nom, la date de naissance, l’identifiant unique, et des liens vers les parents et enfants.
- **Relation** : Un type représentant une relation entre deux individus, soit parent-enfant, soit conjoint-conjoint.

## VI - Tests

Des tests ont été réalisés tout au long du développement afin de garantir la bonne gestion des individus et des relations :
1. **Tests unitaires** : Des tests ont été réalisés pour chaque module, notamment pour l’ajout, la suppression et la modification des individus.
2. **Tests d’intégration** : Des tests ont vérifié que les modules interagissent correctement entre eux.
3. **Tests fonctionnels** : Ils ont permis de vérifier le bon fonctionnement de l’interface utilisateur (si applicable) et la gestion des relations au sein de l’arbre.

## VII - Difficultés et solutions

### Difficultés

- **Gestion de la mémoire** : Ada, étant un langage fortement typé et avec une gestion stricte de la mémoire, a rendu difficile la gestion dynamique des données pour un arbre généalogique de grande taille.
- **Représentation des relations** : L'algorithme pour maintenir et afficher les relations a dû être optimisé afin d’éviter les doublons et garantir l'intégrité de l’arbre.

### Solutions

- La gestion de la mémoire a été résolue en utilisant des structures de données statiques et dynamiques de manière optimale, afin de gérer efficacement la mémoire tout en respectant les contraintes d'Ada.
- Pour la gestion des relations, des fonctions spécifiques ont été écrites pour gérer les modifications, les suppressions et les ajouts sans introduire d’incohérence dans l'arbre.

## VIII - Bilan technique

Le projet est fonctionnel, mais plusieurs améliorations peuvent être envisagées :
- **Optimisation des algorithmes** : Certaines opérations peuvent être rendues plus efficaces, en particulier la gestion des relations complexes.
- **Visualisation** : Bien que l'application soit fonctionnelle, une meilleure interface graphique pourrait être envisagée à l'avenir, éventuellement en intégrant des outils externes pour la représentation visuelle de l’arbre généalogique.

Le travail a été bien réparti au sein du binôme, chaque membre ayant pris en charge une partie spécifique du projet. Les modules ont été développés de manière indépendante, puis intégrés et testés ensemble.

## IX - Bilan personnel

### Intérêt et répartition des tâches

Ce projet m’a permis d'approfondir mes connaissances en Ada, notamment dans la gestion de la mémoire et des structures de données complexes. J’ai principalement travaillé sur la gestion des relations et l’algorithme d’ajout/suppression d’individus. Mon binôme s'est chargé de la gestion des individus et des tests.

### Temps passé

### Enseignements

J’ai appris à travailler avec Ada dans un contexte de gestion complexe de données, en me concentrant sur l’optimisation des algorithmes et la gestion efficace de la mémoire.
