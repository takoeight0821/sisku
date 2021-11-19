import './App.css';
import React, { useEffect } from 'react';
import ReactMarkdown from 'react-markdown';
import { Controller, useForm } from 'react-hook-form';
import Container from '@mui/material/Container';
import Stack from '@mui/material/Stack';
import Input from '@mui/material/Input';
import Paper from '@mui/material/Paper';
import Typography from '@mui/material/Typography';
import { Hover, Location, LocationLink } from 'vscode-languageserver-types';

/**
 * Moniker uniqueness level to define scope of the moniker.
 */
enum UniquenessLevel {
  /**
   * The moniker is only unique inside a document
   */
  document = 'document',

  /**
   * The moniker is unique inside a project for which a dump got created
   */
  project = 'project',

  /**
   * The moniker is unique inside the group to which a project belongs
   */
  group = 'group',

  /**
   * The moniker is unique inside the moniker scheme.
   */
  scheme = 'scheme',

  /**
   * The moniker is globally unique
   */
  global = 'global'
}

/**
 * The moniker kind.
 */
enum MonikerKind {
  /**
   * The moniker represent a symbol that is imported into a project
   */
  import = 'import',

  /**
   * The moniker represents a symbol that is exported from a project
   */
  export = 'export',

  /**
   * The moniker represents a symbol that is local to a project (e.g. a local
   * variable of a function, a class not visible outside the project, ...)
   */
  local = 'local'
}

/**
 * Moniker definition to match LSIF 0.5 moniker definition.
 */
interface Moniker {
  /**
   * The scheme of the moniker. For example tsc or .Net
   */
  scheme: string;

  /**
   * The identifier of the moniker. The value is opaque in LSIF however
   * schema owners are allowed to define the structure if they want.
   */
  identifier: string;

  /**
   * The scope in which the moniker is unique
   */
  unique: UniquenessLevel;

  /**
   * The moniker kind if known.
   */
  kind?: MonikerKind;
}

type SearchResult = {
  hover: Hover;
  definitions: Location[] | LocationLink[] | null;
  moniker: Moniker[] | null;
};

// Get markdown string from Hover.
const getMarkdownString = (hover: Hover): string => {
  if (typeof hover.contents === "string") {
    return hover.contents;
  }
  if ('language' in hover.contents) {
    return '```' + hover.contents.language + '\n' + hover.contents.value + '\n```';
  }
  if (Array.isArray(hover.contents)) {
    return hover.contents.join("\n");
  }
  return hover.contents.value;
}

function App() {
  const endpoint = "http://localhost:8081/search"
  const [query, setQuery] = React.useState('');
  const [searchResults, setSearchResults] = React.useState([] as SearchResult[]);

  const { handleSubmit, control } = useForm();
  const onSubmit = (data: { query: string }) => {
    console.log(data);
    setQuery(data.query);
  };

  useEffect(() => {
    if (query.length > 0) {
      fetch(endpoint + "?q=" + query, { mode: 'cors' })
        .then(response => response.json())
        .then(data => { console.log(data); return data; })
        .then(data => setSearchResults(data))
        .catch(error => {
          console.error(error)
          setSearchResults([])
        })
    }
  }, [query]);

  return (
    <Container sx={{ paddingTop: 2 }}>
      <form onSubmit={handleSubmit(onSubmit)}>
        <Controller
          control={control}
          name="query"
          defaultValue=""
          render={({ field }) => <Input {...field} />}
        />
        <input type="submit" />
      </form>
      <Typography>{query.length > 0 ? `Searching for ${query}` : 'Please enter a search term'}</Typography>
      <Stack spacing={2}>
        {searchResults.map((searchResult: SearchResult, index) =>
          <Paper variant="outlined">
            <Typography variant="subtitle1" sx={{ color: 'primary.main' }}>Result {index}</Typography>
            <Typography variant="subtitle2" sx={{ color: 'secondary.main' }}>Hovers</Typography>
            <ReactMarkdown className="markdown" children={getMarkdownString(searchResult.hover)} />
            <Typography variant="subtitle2" sx={{ color: 'secondary.main' }}>Definition</Typography>
            <Typography>{JSON.stringify(searchResult.definitions, null, 2)}</Typography>
            <Typography variant="subtitle2" sx={{ color: 'secondary.main' }}>Moniker</Typography>
            <Typography>{JSON.stringify(searchResult.moniker, null, 2)}</Typography>
          </Paper>
        )}
      </Stack>
    </Container>
  );
}

export default App;
