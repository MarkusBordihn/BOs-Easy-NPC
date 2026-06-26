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

import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DialogOptionsDataTest {

  @Test
  @DisplayName("Legacy constructor should default the button condition mode to the config value")
  void testLegacyConstructorUsesConfigDefault() {
    DialogOptionsData data = new DialogOptionsData(true, true, true, null, null, null);
    assertEquals(DialogButtonConditionMode.LOCK, data.buttonConditionMode());
  }

  @Test
  @DisplayName("Should not persist the button condition mode when it matches the config default")
  void testSaveOmitsDefaultButtonConditionMode() {
    DialogOptionsData data =
        new DialogOptionsData(true, true, true, null, null, null, DialogButtonConditionMode.LOCK);
    CompoundTag tag = data.createTag();
    assertFalse(tag.contains(DialogOptionsData.DATA_BUTTON_CONDITION_MODE_TAG));
  }

  @Test
  @DisplayName("Should persist the button condition mode when it differs from the config default")
  void testSavePersistsNonDefaultButtonConditionMode() {
    DialogOptionsData data =
        new DialogOptionsData(true, true, true, null, null, null, DialogButtonConditionMode.HIDE);
    CompoundTag tag = data.createTag();
    assertTrue(tag.contains(DialogOptionsData.DATA_BUTTON_CONDITION_MODE_TAG));
    assertEquals(
        DialogButtonConditionMode.HIDE.name(),
        tag.getString(DialogOptionsData.DATA_BUTTON_CONDITION_MODE_TAG).orElse(""));
  }

  @Test
  @DisplayName("Should round-trip a non-default button condition mode through save and load")
  void testSaveLoadRoundTrip() {
    DialogOptionsData original =
        new DialogOptionsData(false, false, false, 10, 20, 30, DialogButtonConditionMode.HIDE);
    DialogOptionsData loaded = DialogOptionsData.load(original.createTag());

    assertEquals(original.allowEscClose(), loaded.allowEscClose());
    assertEquals(original.showCloseButton(), loaded.showCloseButton());
    assertEquals(original.displayAvatar(), loaded.displayAvatar());
    assertEquals(original.avatarTop(), loaded.avatarTop());
    assertEquals(original.avatarLeft(), loaded.avatarLeft());
    assertEquals(original.avatarScale(), loaded.avatarScale());
    assertEquals(original.buttonConditionMode(), loaded.buttonConditionMode());
  }

  @Test
  @DisplayName("Loading a legacy tag without the mode should fall back to the config default")
  void testLoadLegacyTagFallsBackToConfigDefault() {
    DialogOptionsData loaded = DialogOptionsData.load(new CompoundTag());
    assertEquals(DialogButtonConditionMode.LOCK, loaded.buttonConditionMode());
  }

  @Test
  @DisplayName("Loading an unknown stored mode should fall back to LOCK instead of failing")
  void testLoadUnknownModeFallsBackToLock() {
    CompoundTag tag = new CompoundTag();
    tag.putString(DialogOptionsData.DATA_BUTTON_CONDITION_MODE_TAG, "TOGGLE");
    DialogOptionsData loaded = DialogOptionsData.load(tag);
    assertEquals(DialogButtonConditionMode.LOCK, loaded.buttonConditionMode());
  }

  @Test
  @DisplayName("isDefault should reflect the button condition mode")
  void testIsDefaultConsidersButtonConditionMode() {
    DialogOptionsData defaultData =
        new DialogOptionsData(true, true, true, null, null, null, DialogButtonConditionMode.LOCK);
    DialogOptionsData hiddenData =
        new DialogOptionsData(true, true, true, null, null, null, DialogButtonConditionMode.HIDE);

    assertTrue(defaultData.isDefault());
    assertFalse(hiddenData.isDefault());
  }
}
