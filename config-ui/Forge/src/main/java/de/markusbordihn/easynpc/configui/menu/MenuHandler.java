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
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.world.inventory.MenuType;

public class MenuHandler implements MenuHandlerInterface {

  private static final Map<ConfigurationType, MenuType<? extends ConfigurationMenu>>
      configurationMenuMap = new EnumMap<>(ConfigurationType.class);
  private static final Map<EditorType, MenuType<? extends EditorMenu>> editorMenuMap =
      new EnumMap<>(EditorType.class);
  private static final Map<CustomMenuType, MenuType<? extends ConfigUIMenu>> customMenuMap =
      new EnumMap<>(CustomMenuType.class);

  private static synchronized void initializeConfigurationMaps() {
    if (!configurationMenuMap.isEmpty()) {
      return;
    }

    ModMenuTypes.CONFIGURATION_MENU_TYPES.forEach((configurationType, menuType) ->
        configurationMenuMap.put(configurationType, menuType.get()));
  }

  private static synchronized void initializeEditorMaps() {
    if (!editorMenuMap.isEmpty()) {
      return;
    }

    ModMenuTypes.EDITOR_MENU_TYPES.forEach(
        (editorType, menuType) -> editorMenuMap.put(editorType, menuType.get()));
  }

  private static synchronized void initializeCustomMaps() {
    if (!customMenuMap.isEmpty()) {
      return;
    }

    ModMenuTypes.CUSTOM_MENU_TYPES.forEach(
        (customMenuType, menuType) -> customMenuMap.put(customMenuType, menuType.get()));
  }

  @Override
  public MenuType<? extends ConfigurationMenu> getMenuTypeByConfigurationType(
      ConfigurationType configurationType) {
    if (configurationMenuMap.isEmpty()) {
      initializeConfigurationMaps();
    }
    return configurationMenuMap.get(configurationType);
  }

  @Override
  public MenuType<? extends EditorMenu> getMenuTypeByEditorType(EditorType editorType) {
    if (editorMenuMap.isEmpty()) {
      initializeEditorMaps();
    }
    return editorMenuMap.get(editorType);
  }

  @Override
  public MenuType<? extends ConfigUIMenu> getMenuTypeByCustomType(CustomMenuType customMenuType) {
    if (customMenuMap.isEmpty()) {
      initializeCustomMaps();
    }
    return customMenuMap.get(customMenuType);
  }
}
