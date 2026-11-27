/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.menu;

import de.markusbordihn.easynpc.configui.data.custom.CustomMenuType;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import net.minecraft.world.inventory.MenuType;

public class MenuHandler implements MenuHandlerInterface {

  @Override
  public MenuType<? extends ConfigurationMenu> getMenuTypeByConfigurationType(
      ConfigurationType configurationType) {
    return ModMenuTypes.CONFIGURATION_MENU_TYPES.get(configurationType);
  }

  @Override
  public MenuType<? extends EditorMenu> getMenuTypeByEditorType(EditorType editorType) {
    return ModMenuTypes.EDITOR_MENU_TYPES.get(editorType);
  }

  @Override
  public MenuType<? extends ConfigUIMenu> getMenuTypeByCustomType(CustomMenuType customMenuType) {
    return ModMenuTypes.CUSTOM_MENU_TYPES.get(customMenuType);
  }
}
