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

import de.markusbordihn.easynpc.Constants;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record ScreenData(
    UUID uuid, UUID dialogId, UUID dialogButtonId, int pageIndex, CompoundTag additionalData) {

  public static final ScreenData EMPTY = new ScreenData(null, null, null, 0, new CompoundTag());
  public static final String SCREEN_DATA_ADDITIONAL_DATA_TAG = "AdditionalData";
  public static final String SCREEN_DATA_DIALOG_BUTTON_ID_TAG = "DialogButtonId";
  public static final String SCREEN_DATA_DIALOG_ID_TAG = "DialogID";
  public static final String SCREEN_DATA_PAGE_INDEX_TAG = "PageIndex";
  public static final String SCREEN_DATA_TAG = "ScreenData";
  public static final String SCREEN_DATA_UUID_TAG = "UUID";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public static ScreenData decode(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(SCREEN_DATA_TAG)) {
      log.error("Unable to decode screen data from compound tag: {}", compoundTag);
      return null;
    }
    CompoundTag screenDataTag = compoundTag.getCompound(SCREEN_DATA_TAG);
    UUID uuid = screenDataTag.getUUID(SCREEN_DATA_UUID_TAG);
    UUID dialogID =
        screenDataTag.contains(SCREEN_DATA_DIALOG_ID_TAG)
            ? screenDataTag.getUUID(SCREEN_DATA_DIALOG_ID_TAG)
            : null;
    UUID dialogButtonId =
        screenDataTag.contains(SCREEN_DATA_DIALOG_BUTTON_ID_TAG)
            ? screenDataTag.getUUID(SCREEN_DATA_DIALOG_BUTTON_ID_TAG)
            : null;
    int pageIndex = screenDataTag.getInt(SCREEN_DATA_PAGE_INDEX_TAG);
    CompoundTag data =
        screenDataTag.contains(SCREEN_DATA_ADDITIONAL_DATA_TAG)
            ? screenDataTag.getCompound(SCREEN_DATA_ADDITIONAL_DATA_TAG)
            : new CompoundTag();
    return new ScreenData(uuid, dialogID, dialogButtonId, pageIndex, data);
  }

  public CompoundTag encode() {
    CompoundTag screenDataTag = new CompoundTag();
    screenDataTag.putUUID(SCREEN_DATA_UUID_TAG, this.uuid);
    if (this.dialogId != null) {
      screenDataTag.putUUID(SCREEN_DATA_DIALOG_ID_TAG, this.dialogId);
    }
    if (this.dialogButtonId != null) {
      screenDataTag.putUUID(SCREEN_DATA_DIALOG_BUTTON_ID_TAG, this.dialogButtonId);
    }
    screenDataTag.putInt(SCREEN_DATA_PAGE_INDEX_TAG, this.pageIndex);
    if (this.additionalData != null) {
      screenDataTag.put(SCREEN_DATA_ADDITIONAL_DATA_TAG, this.additionalData);
    }

    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(SCREEN_DATA_TAG, screenDataTag);

    return compoundTag;
  }
}
