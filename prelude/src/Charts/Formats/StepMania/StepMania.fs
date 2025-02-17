namespace Prelude.Charts.Formats.StepMania

type StepMania() =
    static member FromFile(path: string) = StepmaniaParser.parse_file path