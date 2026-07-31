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
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class AdditionalScreenDataTest {

  @Test
  @DisplayName("Reading the screen data leaves the tag of the caller untouched")
  void testConstructorDoesNotStripTheSourceTag() {
    CompoundTag sourceTag = new CompoundTag();
    AdditionalScreenData.addActionEventType(sourceTag, ActionEventType.ON_INTERACTION);
    sourceTag.putString("CustomKey", "custom value");

    AdditionalScreenData additionalScreenData = new AdditionalScreenData(sourceTag);

    assertEquals(
        ActionEventType.ON_INTERACTION, AdditionalScreenData.getActionEventType(sourceTag));
    assertEquals("custom value", sourceTag.getString("CustomKey"));
    assertEquals(ActionEventType.ON_INTERACTION, additionalScreenData.getActionEventType());
  }

  @Test
  @DisplayName("Processed keys are removed from the screen data, unknown keys are kept")
  void testProcessedKeysAreRemovedFromTheScreenData() {
    CompoundTag sourceTag = new CompoundTag();
    AdditionalScreenData.addActionEventType(sourceTag, ActionEventType.ON_INTERACTION);
    sourceTag.putString("CustomKey", "custom value");

    AdditionalScreenData additionalScreenData = new AdditionalScreenData(sourceTag);

    assertEquals("custom value", additionalScreenData.getData().getString("CustomKey"));
    assertEquals(
        ActionEventType.NONE,
        AdditionalScreenData.getActionEventType(additionalScreenData.getData()));
  }
}
