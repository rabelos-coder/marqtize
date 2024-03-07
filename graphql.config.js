module.exports = {
  schema: "http://localhost:4000/graphql",
  extensions: {
    languageService: {
      cacheSchemaFileForLookup: false,
      enableValidation: true,
    },
  },
};
