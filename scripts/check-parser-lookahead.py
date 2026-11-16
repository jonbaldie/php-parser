#!/usr/bin/env python3
"""Keep the raw Megaparsec lookahead inside the session-restoring wrapper."""

from pathlib import Path
import re
import sys

root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).resolve().parent.parent
source_root = root / 'src'
parser = source_root / 'Language/PHP/Parser'
if not (parser / 'Lexer.hs').is_file():
    sys.exit(f'{parser}: parser source not found')
allowed = {
    'Language/PHP/Parser/Lexer.hs': {
        'import qualified Text.Megaparsec as M hiding (lookAhead)',
        'import qualified Text.Megaparsec as Raw (lookAhead)',
    },
    'Language/PHP/Parser/Megaparsec.hs': {'import Text.Megaparsec as Megaparsec hiding (lookAhead)'},
    'Language/PHP/Parser/Name.hs': {'import qualified Text.Megaparsec as M'},
    'Language/PHP/Parser/Error.hs': {'import Text.Megaparsec'},
}
errors = []
for path in sorted(source_root.rglob('*.hs')):
    source = path.read_text()
    imports = re.findall(r'^[ \t]*import\s+(?:qualified\s+)?(?:"[^"]+"\s+)?Text\.Megaparsec(?![\w.])[^\n]*', source, re.MULTILINE)
    permitted = allowed.get(path.relative_to(source_root).as_posix(), set())
    for declaration in imports:
        if declaration.strip() not in permitted:
            errors.append(f'{path}: use qualified Language.PHP.Parser.Megaparsec as M instead of {declaration.strip()}')
    if path.relative_to(source_root).as_posix() == 'Language/PHP/Parser/Lexer.hs' and len(re.findall(r'\bRaw\s*\.\s*lookAhead\b', source)) != 1:
        errors.append(f'{path}: use the state-restoring Lexer.lookAhead; Raw.lookAhead belongs only in its wrapper')
    if path.relative_to(source_root).as_posix() == 'Language/PHP/Parser/Name.hs' and re.search(r'\bM\s*\.\s*lookAhead\b', source):
        errors.append(f'{path}: use the state-restoring Lexer.lookAhead in a parser module')
    error_import = '''import Text.Megaparsec
  ( ParseErrorBundle (..)
  , PosState (..)
  , SourcePos (..)
  , reachOffsetNoLine
  , unPos
  )'''
    if path.relative_to(source_root).as_posix() == 'Language/PHP/Parser/Error.hs' and error_import not in source:
        errors.append(f'{path}: keep the explicit diagnostic-only Megaparsec import')

if errors:
    print('\n'.join(errors), file=sys.stderr)
    sys.exit(1)
print('Parser lookahead imports are safe')
