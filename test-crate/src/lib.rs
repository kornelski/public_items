pub struct WithGenericArgs<'a, 'b: 'a, U, T = usize> {
    pub field: (&'a String, &'b usize, T, U),
}
