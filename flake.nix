{
  outputs = { self }: {
    templates.default = {
      description = "A Haskell project";
      path = ./template;
    };
  };
}
