/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.data.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogDataSetTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  @DisplayName("An NPC without dialogs stores no dialog list")
  void testEmptyDialogSetStoresNoList() {
    CompoundTag compoundTag = new DialogDataSet().createTag();

    assertFalse(compoundTag.contains(DialogDataSet.DATA_DIALOG_DATA_SET_TAG));
    assertEquals(DialogType.NONE.name(), compoundTag.getString(DialogDataSet.DATA_TYPE_TAG));
  }

  @Test
  @DisplayName("A dialog survives a round trip")
  void testDialogSurvivesRoundTrip() {
    DialogDataSet dialogDataSet = new DialogDataSet(DialogType.BASIC);
    dialogDataSet.addDialog(new DialogDataEntry("welcome", "Welcome", "Hello there!"));

    DialogDataSet restored = new DialogDataSet(dialogDataSet.createTag());

    assertTrue(restored.hasDialog("welcome"));
    assertEquals("Hello there!", restored.getDialog("welcome").getText());
    assertEquals(DialogType.BASIC, restored.getType());
  }

  @Test
  @DisplayName("A basic dialog set with several dialogs becomes a standard one")
  void testDialogTypeIsResolvedOnSave() {
    DialogDataSet dialogDataSet = new DialogDataSet(DialogType.BASIC);
    dialogDataSet.addDialog(new DialogDataEntry("first", "First", "First dialog"));
    dialogDataSet.addDialog(new DialogDataEntry("second", "Second", "Second dialog"));

    assertEquals(
        DialogType.STANDARD.name(),
        dialogDataSet.createTag().getString(DialogDataSet.DATA_TYPE_TAG));
    assertEquals(DialogType.STANDARD, new DialogDataSet(dialogDataSet.createTag()).getType());
  }

  @Test
  @DisplayName("A dialog set which lost its last dialog is stored as none")
  void testEmptiedDialogSetIsStoredAsNone() {
    DialogDataSet dialogDataSet = new DialogDataSet(DialogType.BASIC);
    DialogDataEntry dialogDataEntry = new DialogDataEntry("welcome", "Welcome", "Hello there!");
    dialogDataSet.addDialog(dialogDataEntry);
    dialogDataSet.removeDialog(dialogDataEntry.getId());

    CompoundTag compoundTag = dialogDataSet.createTag();

    assertFalse(compoundTag.contains(DialogDataSet.DATA_DIALOG_DATA_SET_TAG));
    assertEquals(DialogType.NONE.name(), compoundTag.getString(DialogDataSet.DATA_TYPE_TAG));
  }

  @Test
  @DisplayName("Storing a dialog set does not change the dialog type of the editor")
  void testSaveKeepsTheDialogTypeOfTheSet() {
    DialogDataSet dialogDataSet = new DialogDataSet(DialogType.BASIC);
    dialogDataSet.addDialog(new DialogDataEntry("first", "First", "First dialog"));
    dialogDataSet.addDialog(new DialogDataEntry("second", "Second", "Second dialog"));

    dialogDataSet.createTag();

    assertEquals(DialogType.BASIC, dialogDataSet.getType());
  }
}
