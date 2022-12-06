# CancerDataServices-Stat_GeneratoR
This takes a validated indexed CDS template, and/or the newest version of dbGaP submission for the study.

To run the script on a CDS template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CDS-Stat_GeneratoR.R --help
```

```
Usage: CDS-Stat_GeneratoR.R [options]

CDS-Stat_GeneratoR v2.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		A validated and indexed CDS template file (.xlsx, .tsv, .csv)

	-c CHARACTER, --subject_consent=CHARACTER
		A dbGaP subject_consent data file (SC_DS)

	-a CHARACTER, --sample_attribute=CHARACTER
		A dbGaP sample_attribute data file (SA_DS)

	-h, --help
		Show this help message and exit
```

An example set of files have been provided. They can all be used at once or any other combination of the test files:

```
Rscript --vanilla CDS-Stat_GeneratoR.R -f test_files/a_all_pass-v1.3.1_index20221206.tsv -c test_files/set_b/SC_DS_b.txt -a test_files/set_b/SA_DS_b.txt
```

```
The data file is being validated at this time.
This is a validation output for a_all_pass-v1.3.1_index20221206.



Process Complete.

The output file can be found here: CancerDataServices-Stat_GeneratoR/test_files/
```
