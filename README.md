# LCDMV

(c) 2016 Hiroshi Noji.

This is an implementation for several grammar induction models described in the the paper:

> Hiroshi Noji, Yusuke Miayo, and Mark Johnson. 2016. [Using Left-corner Parsing to Encode Universal Structural Constraints in Grammar Induction](http://aclweb.org/anthology/D/D16/D16-1004.pdf). In Proceedings of the 2016 Conference on Empirical Methods in Natural Language Processing.

The models include DMV (dependency model with valence) and several extensions such as our structurally constrained model on the maximum degree of center-embedding using left-corner parsing.

## Setup: Universal Dependencies

The primary datasets for experiment in LCDMV are [Universal Dependencies (UD)](http://universaldependencies.org).

We assume for training and testing the input data is one of UD treebank, but it should be modified so that it follows the conventional CoNLL-X format. Specifically,

1) The comment lines strating with # must be removed; and
2) The word annotation lines (e.g., `2-3   haven't`) must also be removed.

We also recommend to remove all punctuations prior to experiments.

The scripts `script/to_conllx.py` and `script/remove_punct.py` performs these preprocessing. They can be used as:

``` shell
cat /xxx/UD_French/fr-ud-train.conllu | script/to_conllx.py | script/remove_punct.py > fr-ud-train-nopunct.conllx
```

All components in the system are included in the pre-assembled jar file `lcdmv.jar`.
You can also build your own jar by `./bin/sbt assembly`.

## Learning

### Baseline

The following command performs learning of the baseline featurized-DMV model with no structural constraints (except the UD function words constraint):

``` shell
java -jar lcdmv.jar -actionType featureTrain -numIters 100 -trainPath /xxx/yyy-train.conllx \
-maxLength 15 -posdef cpos -numThreads 0 -unheadPOS udfunc -saveModelPath zzz-model.ser
```

The software supports many other options, which can be shown with `java -jar lcdmv.jar -help`.
Note that not all options may be compatible with others.

E-step in EM is performed in parallel, and `-numThreads` specifies the number of threads in this computing.
`0` is to use the maximum number of threads allowed (depending on the number of cores in the machine).

### Constraints on left-corner stack depth

The following command reproduces the results with the constraint that we found the best:
the maximum number of embedded tokens up to 3:

``` shell
java -jar lcdmv.jar -actionType featureTrain -numIters 100 -trainPath /xxx/yyy-train.conllx \
-maxLength 15 -posdef cpos -numThreads 0 -unheadPOS udfunc -saveModelPath zzz-model.ser \
-maxDepth 1 -maxChunkSize 3
```

where `-maxDepth 1 -maxChunkSize 3` means the stack depth >= 2 in the chart will be *conditionally*
prohibited, in that it is allowed when the number of tokens in the embedded chunk is up to 3.

### Dependency length

For the another baseline of dependency length constraint, the following setting reproduces
the results:

``` shell
java -jar lcdmv.jar -actionType featureTrain -numIters 100 -trainPath /xxx/yyy-train.conllx \
-maxLength 15 -posdef cpos -numThreads 0 -unheadPOS udfunc -saveModelPath zzz-model.ser \
-distanceBeta 0.1
```

## Parsing

After learning, the following command outputs the predicted trees on test sentences and calculate
(UAS) score against the gold treebank. The score is outputted in the standard output.

``` shell
java -jar lcdmv.jar -actionType evaluate -testPath /xxx/yyy-test.conllx
-maxLength 40 -posdef cpos -loadModelPath zzz-model.ser -outputPath zzz-guess.conllx
```

## Citation

If you use the code in research publications, please cite:

```
@InProceedings{noji-miyao-johnson:2016:EMNLP2016,
  author    = {Noji, Hiroshi  and  Miyao, Yusuke  and  Johnson, Mark},
  title     = {Using Left-corner Parsing to Encode Universal Structural Constraints in Grammar Induction},
  booktitle = {Proceedings of the 2016 Conference on Empirical Methods in Natural Language Processing},
  month     = {November},
  year      = {2016},
  address   = {Austin, Texas},
  publisher = {Association for Computational Linguistics},
  pages     = {33--43},
  url       = {https://aclweb.org/anthology/D16-1004}
}
```

## Others

The paper above also used our hacked version of Naseem et al. (2010)'s grammar induction system, which can be available [here](https://github.com/hiroshinoji/naseem_et_al_2010).
