job:
  name:        {{ .JobName }}
  description: "Export VSAM → CSV pipeline"
  account:     ACC123 
  class:       A
  msgclass:    X
  region:      4M
  parm:        "PARM=FOO"

  # z/OSMF & cloud contexts
  env:
    zosmf:
      url:   https://mainframe.your.org:1443
      user:  ${ZOS_USER}
      token: ${ZOS_TOKEN}
    aws:
      profile: job-dev

# global defaults
datasets:
  default:
    unit: SYSDA
    space:
      primary:   1
      secondary: 1
    recfm: FB
    lrecl:  80

# local filesystem ↔ dataset mounts
inputs:
  - name: INFILE
    dsname: VSAM.CUSTOMERS
    copybook: ./copybooks/customer.cpy
  - name: CONFIG
    path: ./config/params.json

outputs:
  - name: CSV_OUT
    path: ./data/customers.csv

steps:
  - id: build
    name: "Compile Grace code"
    exec:
      command: grace build ./records.grc -o ./build

  - id: extract
    name: "VSAM → CSV"
    exec:
      command: |
        grace data extract \
          --input INFILE \
          --copybook copybooks/customer.cpy \
          --output CSV_OUT

  - id: allocate
    name: "Allocate sequential dataset"
    dd:
      name: MYCSV
      dsname: NEW.CSV.OUTPUT
      disp: "(NEW,CATLG,DELETE)"
      unit: SYSDA
      space:
        primary: 1
        secondary: 1
      recfm: FB
      lrecl: 100

  - id: jcl_submit
    name: Submit JCL
    jcl:
      template: ./jcl/transform.jcl.tpl
      params:
        INFILE: "${inputs.INFILE.dsname}"
        OUTDS:  "${steps.allocate.dd.dsname}"
      allocateFrom: datasets.default

  - id: deliver
    name: "Push to S3"
    onSuccess:
      hopper:
        to:
          - s3://your-mainframe-exports/customers/${job.name}-${timestamp}.csv
          - kafka://your.org.vsam.exports/customers

# global hooks and retry policies
retry:
  attempts: 2
  backoff: 5s

notifications:
  onFailure:
    email: ops@your.org
    slack: "#mainframe-alerts"
