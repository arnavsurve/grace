# grace.yml – job configuration
job:
  name: {{ .JobName }}
  input: data/input.txt
  output: data/output.txt
jcl:
  ddname: MYDD
