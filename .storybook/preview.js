
import '../elm-stuff/gitdeps/github.com/unisonweb/ui-core/src/css/code.css'
import '../elm-stuff/gitdeps/github.com/unisonweb/ui-core/src/css/ui.css'

import '../static/style.css'
import "../elm-stuff/gitdeps/github.com/unisonweb/ui-core/src/css/themes/unison-light.css";

export const parameters = {
    actions: { argTypesRegex: "^on[A-Z].*" },
    options: {
        storySort: {
            order: [
                'Basics',
                ['Typography'],
                'Layout',
                ['Row', 'Column'],
                'Button',
                ['Primary', 'Secondary', 'Danger', 'Disabled']
            ]
        }
    }
}