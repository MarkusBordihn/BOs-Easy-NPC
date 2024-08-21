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
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.neoforged.neoforge.gametest.GameTestHolder;
import net.neoforged.neoforge.gametest.PrefixGameTestTemplate;

@SuppressWarnings("unused")
@PrefixGameTestTemplate(value = false)
@GameTestHolder(Constants.MOD_ID)
public class EditorScreenTest {

  @GameTest(template = "gametest.3x3x3")
  public void testActionDataEditorScreen(GameTestHelper helper) {
    EditorScreenTestHelper.testEditorScreen(
        helper,
        ModEntityType.HUMANOID.get(),
        EditorType.ACTION_DATA,
        ModMenuTypes.ACTION_DATA_EDITOR_MENU.get());
  }

  @GameTest(template = "gametest.3x3x3")
  public void testActionDataEntryEditorScreen(GameTestHelper helper) {
    EditorScreenTestHelper.testEditorScreen(
        helper,
        ModEntityType.HUMANOID.get(),
        EditorType.ACTION_DATA_ENTRY,
        ModMenuTypes.ACTION_DATA_ENTRY_EDITOR_MENU.get());
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogEditorScreen(GameTestHelper helper) {
    EditorScreenTestHelper.testEditorScreen(
        helper,
        ModEntityType.HUMANOID.get(),
        EditorType.DIALOG,
        ModMenuTypes.DIALOG_EDITOR_MENU.get());
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogButtonEditorScreen(GameTestHelper helper) {
    EditorScreenTestHelper.testEditorScreen(
        helper,
        ModEntityType.HUMANOID.get(),
        EditorType.DIALOG_BUTTON,
        ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU.get());
  }

  @GameTest(template = "gametest.3x3x3")
  public void testDialogTextEditorScreen(GameTestHelper helper) {
    EditorScreenTestHelper.testEditorScreen(
        helper,
        ModEntityType.HUMANOID.get(),
        EditorType.DIALOG_TEXT,
        ModMenuTypes.DIALOG_TEXT_EDITOR_MENU.get());
  }
}
