package eg.basic

trait BASIC extends Interp with DSL {
    def apply() = run(prog)
}
