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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.menu.MenuHandlerInterface;
import de.markusbordihn.easynpc.menu.MenuManager;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraftforge.gametest.GameTestHolder;
import net.minecraftforge.gametest.PrefixGameTestTemplate;

@SuppressWarnings("unused")
@PrefixGameTestTemplate(value = false)
@GameTestHolder(Constants.MOD_ID)
public class MenuManagerTest {

  @GameTest(template = "gametest.1x1x1")
  public void testMissingConfigurationType(GameTestHelper helper) {
    MenuHandlerInterface menuHandlerInterface = MenuManager.getMenuHandler();
    for (ConfigurationType configurationType : ConfigurationType.values()) {
      if (configurationType == ConfigurationType.NONE || configurationType.isAlias()) {
        continue;
      }
      GameTestHelpers.assertNotNull(
          helper,
          "Menu type for configuration type " + configurationType + " is missing!",
          menuHandlerInterface.getMenuTypeByConfigurationType(configurationType));
    }
    helper.succeed();
  }

  @GameTest(template = "gametest.1x1x1")
  public void testMissingEditorType(GameTestHelper helper) {
    MenuHandlerInterface menuHandlerInterface = MenuManager.getMenuHandler();
    for (EditorType editorTypeType : EditorType.values()) {
      if (editorTypeType == EditorType.NONE) {
        continue;
      }
      GameTestHelpers.assertNotNull(
          helper,
          "Menu type for editor type " + editorTypeType + " is missing!",
          menuHandlerInterface.getMenuTypeByEditorType(editorTypeType));
    }
    helper.succeed();
  }
}
