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

package de.markusbordihn.easynpc.data.screen;

import static org.junit.jupiter.api.Assertions.*;

import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonReference;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AdditionalScreenDataTest {

  private static Tag encode(DialogButtonReference... references) {
    return DialogButtonReference.LIST_CODEC
        .encodeStart(NbtOps.INSTANCE, List.of(references))
        .result()
        .orElseThrow();
  }

  @Test
  @DisplayName("Reading the screen data leaves the tag of the caller untouched")
  void testConstructorDoesNotStripTheSourceTag() {
    CompoundTag sourceTag = new CompoundTag();
    AdditionalScreenData.addActionEventType(sourceTag, ActionEventType.ON_INTERACTION);
    sourceTag.putString("CustomKey", "custom value");

    AdditionalScreenData additionalScreenData = new AdditionalScreenData(sourceTag);

    assertEquals(
        ActionEventType.ON_INTERACTION, AdditionalScreenData.getActionEventType(sourceTag));
    assertEquals("custom value", sourceTag.getString("CustomKey").orElse(""));
    assertEquals(ActionEventType.ON_INTERACTION, additionalScreenData.getActionEventType());
  }

  @Test
  @DisplayName("Processed keys are removed from the screen data, unknown keys are kept")
  void testProcessedKeysAreRemovedFromTheScreenData() {
    CompoundTag sourceTag = new CompoundTag();
    AdditionalScreenData.addActionEventType(sourceTag, ActionEventType.ON_INTERACTION);
    sourceTag.putString("CustomKey", "custom value");

    AdditionalScreenData additionalScreenData = new AdditionalScreenData(sourceTag);

    assertEquals("custom value", additionalScreenData.getData().getString("CustomKey").orElse(""));
    assertEquals(
        ActionEventType.NONE,
        AdditionalScreenData.getActionEventType(additionalScreenData.getData()));
  }

  @Test
  @DisplayName("A locked button only locks the dialog it belongs to")
  void testDialogButtonLockIsScopedToItsDialog() {
    UUID buttonId = UUIDUtils.textToUUID("five");
    UUID withdrawDialogId = UUID.randomUUID();
    UUID depositDialogId = UUID.randomUUID();
    CompoundTag sourceTag = new CompoundTag();
    sourceTag.put(
        "DialogButtonLockData", encode(new DialogButtonReference(withdrawDialogId, buttonId)));

    AdditionalScreenData additionalScreenData = new AdditionalScreenData(sourceTag);

    assertTrue(additionalScreenData.isDialogButtonLocked(withdrawDialogId, buttonId));
    assertFalse(additionalScreenData.isDialogButtonLocked(depositDialogId, buttonId));
  }

  @Test
  @DisplayName("A reached execution limit only applies to the dialog it belongs to")
  void testExecutionLimitIsScopedToItsDialog() {
    UUID buttonId = UUIDUtils.textToUUID("daily_reward");
    UUID usedDialogId = UUID.randomUUID();
    UUID otherDialogId = UUID.randomUUID();
    CompoundTag sourceTag = new CompoundTag();
    sourceTag.put("ExecutionLimitData", encode(new DialogButtonReference(usedDialogId, buttonId)));

    AdditionalScreenData additionalScreenData = new AdditionalScreenData(sourceTag);

    assertTrue(additionalScreenData.isExecutionLimitReached(usedDialogId, buttonId));
    assertFalse(additionalScreenData.isExecutionLimitReached(otherDialogId, buttonId));
  }

  @Test
  @DisplayName("Button references survive the round trip through the screen data")
  void testButtonReferencesSurviveRoundTrip() {
    DialogButtonReference first = new DialogButtonReference(UUID.randomUUID(), UUID.randomUUID());
    DialogButtonReference second = new DialogButtonReference(UUID.randomUUID(), UUID.randomUUID());
    CompoundTag sourceTag = new CompoundTag();
    sourceTag.put("DialogButtonLockData", encode(first, second));

    assertEquals(Set.of(first, second), AdditionalScreenData.getLockedDialogButtons(sourceTag));
  }

  @Test
  @DisplayName("Lock data in the old label only format is ignored instead of failing")
  void testLegacyLockDataIsIgnored() {
    ListTag legacyLocks = new ListTag();
    legacyLocks.add(
        UUIDUtil.CODEC.encodeStart(NbtOps.INSTANCE, UUIDUtils.textToUUID("five")).getOrThrow());
    CompoundTag sourceTag = new CompoundTag();
    sourceTag.put("DialogButtonLockData", legacyLocks);

    assertTrue(AdditionalScreenData.getLockedDialogButtons(sourceTag).isEmpty());
  }
}
