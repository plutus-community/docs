# 1.	Introduction

Pour exécuter l'instance locale de `Plutus Playground`, pour basculer sur la version du dépôt `GIT` nécessaire du répertoire `plutus-apps` et pour initialiser l'environnement de travail de cette lecture, référez vous au document de la **semaine 1** qui contient toute les informations nécessaires.

# 2. Lecture 3
## 2.1 Objectifs

Les objectifs principaux pour cette 3ème semaine sont expliqués dans la **vidéo de Lars** dédié, **partie 8** : 

- Implémenter les **scripts de validation** qui examinent le **contexte de script**. 
- Examiner le **type** qui définis le **contexte**.
- Etudier comment les utiliser dans la forme de **contrats d'acquisition**. 
- Etudier comment les **contracts sensible au temps** ou la **logique de validation d'un contrat** peuvent être écrits en `Plutus`.
- Etudier comment écrire des **contrats paramétrés**. 
- Apprendre à utiliser le `Cardano CLI` pour intéragir avec des contrats `Plutus` sur `Cardano Testnet`.

## 2.2 Les concepts clés

- `Plutus validator` ou **contrat intelligent Cardano**
- Notions de contexte de script ou `script context`.
- Contrats **paramétrés** vs. **non-paramétrés**. 
- Gestion du temps sur **Cardano**.
- Modèle **eUTxo**.

> Références pour les sections 2.2.x
> - Vidéo Lecture 3 part 2.
> - Vidéo Lecture 3 part 3.
> - https://playground.plutus.iohkdev.io/doc/haddock/
> - https://www.canonicalllc.com/post/plutus-101-from-multisig-to-programmatic-validation
> - https://www.canonicalllc.com/post/plutus-102-datum-and-redeemer

### 2.2.1 Notion de contexte de script

Dans la lecture 3 partie 2, Lars parle du contexte de script :

Quand j'ai expliqué le modèle étendu UtxO (eUtxO) lors de la première lecture, nous avons vu que pour dévérouiller une adresse de script, le script attaché à cette adresse doit être alors exécuté. 
Et ce script prend 3 morceaux de l'entrée, le datum, le redeemer et le contexte. 

Puis dans la deuxième lecture, nous avons vu des exemples de mise en oeuvre en **Haskell**. 
Nous avons vu l'implémentation de bas niveau où les 3 arguments, **datum**, **redeemer** et **contexte** sont représenté par le type `data`. 
Mais j'ai aussi mentionné qu'en pratique seulement la version typé est utilisée, où **datum** et **redeemer** peuvent être des types personnalisés tant qu'ils implémentent `data class type`  et que le troisième argument, le contexte est de type `script context`.
L'example que nous avons vu examine seulement le **datum** et le **redeemer**, mais nous ignorons toujours le **contexte**.

Alors dans cette lecture, nous voulons commencer à regarder le **contexte** et ce type `ScriptContext` est défini dans le package `plutus-ledger-api` et le **contexte** est dans le module `Plutus.V1.Ledger.Contexts`.

Dans le modèle UTxO utilisé par Cardano, le **contexte de validation** est la **transaction qui dépense**, c'est à dire la transaction elle-même, la liste des **entrées** et des **sorties** de cette transaction. 

**Complément d'information sur le script de contexte**

Il y a un choix varié de ce que peut être le contexte d'un script de contexte. 
Il peut être très restreint, contenant seulement le **redeemer** ou il peut être très global incluant tout l'état de la **blockchain**.
Dans Cardano, le contexte de script est la transaction en train d'être validée, incluant toutes ses entrées et ses sorties.

### 2.2.2 Contrats intelligents Plutus alias Validator Scripts.

- Les **Validator Scripts** sont hachés pour créer une adresse de script. 
- les actifs sont envoyés à l'adresse de contrat intelligent et y sont **vérouillés**.
- les actifs doivent passer le **validateur Plutus** pour quitter l'adresse de script et être **dévérouillé**. 
- Il y a 3 éléments d'entrée dans un validateur Plutus : **datum**, **redeemer** et le **contexte de script**. 
- Le **datum** est l'entrée du **locker**.
- Le **redeemer** est l'entrée du **unlocker** de l'entrée. 
- Le **locker** envoie un hachage du **datum** lors du verrouillage. 
- Le **unlocker** doit considérer le **datum** que le **locker** utilise pour créer le hache.
- Le **unlocker** doit envoyer le **datum** correspondant au hash et l'éventuel **redeemer** lors du déverrouillage.

### 2.2.3 Gestion du temps et validité d'une transaction.

Lars parle de la gestion du temps et de la validité vs. échec d'une transaction dans la lecture 3, partie 3. 
Un des gros avantages que le modèle UTxO de Cardano possède par rapport à quelque chose comme Etherum est le fait que la validation peut être faite dans le portefeuille. 

Comme je l'avais expliqué précédement, les transactions peuvent encore échouer quand elles arrivent dans la blockchain au niveau du noeud de validation à cause d'une entrée qui a déjà été consommé entre temps par quelqu'un d'autre.  
Dans ce cas, la transaction échoue tout simplement sans avoir de frais à payer. 
Mais ce qui ne peut jamais ou ne doit jamais arriver dans des circonstances normales, c'est qu'un scripte de validation se lance et ensuite échoue.
Parce que vous pouvez déjà exécuter le script exactement dans les mêmes conditions dans le portefeuille et vous verrez donc qu'il échouerait avant même que vous ne le soumettiez. Et c'est une fonctionnalité très importante et très agréable.
Mais si vous y réfléchissez, la gestion du temps dans ce contexte n'est pas claire. Le temps est évidemment important, parce que nous voulons pouvoir exprimer une logique de validation qui dit qu'une certaine transaction n'est valide qu'après un certain temps ou avant qu'un certain temps ne soit écoulé.

Nous en avons vu un exemple dans le tout premier exemple de l'enchère, les offres ne sont autorisées que durant un intervalle de temps donné.
 Et le point d'accès `close` ne peut être appelé qu'après l'expiration du délai. Et en y réfléchissant, cela semble être une contradiction. Car le temps s'écoule sans cesse. Ainsi, lorsque vous essayez de valider une transaction que vous construisez dans votre portefeuille, l'heure à laquelle vous le faites dans le portefeuille peut bien sûr être différente de l'heure à laquelle la transaction arrive à un nœud pour validation. On ne sait donc pas comment réunir ces deux éléments pour d'une part gérer le temps, mais d'autre part garantir que la validation soit déterministe dans le sens que si elle ne réussit que dans le portefeuille, elle réussira également dans le nœud.

**Cardano** résout ce problème en ajoutant un champ `POSIX` de plage de temps, le champ `txInfoValidRange`, à une transaction. 
Ce champ c'est un intervalle de temps valide.

Il indique donc que cette transaction est valide entre telle et telle heure. cela est spécifié dans la transaction. Quand elle est envoyée à la blockchain et validée par un nœud. Ensuite, avant que les scripts ne soient exécutés, certaines vérifications générales sont effectuées. 



Par exemple, que toutes les entrées sont présentes et que les soldes s'additionnent et ainsi de suite. Que les frais sont inclus. Et l'une de ces vérifications qui se produit avant la validation est que la plage de temps est vérifiée. Ainsi, lorsqu'un nœud valide, l'un de ces contrôles préalables à la validation consiste à vérifier l'heure actuelle et à la comparer à la plage horaire spécifiée dans la transaction. Et si l'heure actuelle n'entre pas dans cette plage horaire, la validation échoue immédiatement sans jamais exécuter les scripts du validateur. Mais cela signifie également que si ces vérifications préalables réussissent, nous pouvons supposer que l'heure actuelle se situe dans cet intervalle. La validation est alors à nouveau complètement déterministe. Il s'agit simplement d'un élément de données statique attaché à la transaction. 

