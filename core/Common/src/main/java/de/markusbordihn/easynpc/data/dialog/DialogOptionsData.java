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

package de.markusbordihn.easynpc.data.dialog;

import de.markusbordihn.easynpc.config.DialogOptionsConfig;
import net.minecraft.nbt.CompoundTag;

public record DialogOptionsData(
    boolean allowEscClose,
    boolean showCloseButton,
    boolean displayAvatar,
    Integer avatarTop,
    Integer avatarLeft,
    Integer avatarScale,
    DialogButtonConditionMode buttonConditionMode) {

  public static final String DATA_ALLOW_ESC_CLOSE_TAG = "AllowEscClose";
  public static final String DATA_DISPLAY_AVATAR_TAG = "DisplayAvatar";
  public static final String DATA_SHOW_CLOSE_BUTTON_TAG = "ShowCloseButton";
  public static final String DATA_AVATAR_LEFT_TAG = "AvatarLeft";
  public static final String DATA_AVATAR_SCALE_TAG = "AvatarScale";
  public static final String DATA_AVATAR_TOP_TAG = "AvatarTop";
  public static final String DATA_BUTTON_CONDITION_MODE_TAG = "ButtonConditionMode";

  public static final DialogOptionsData DEFAULT =
      new DialogOptionsData(true, true, true, null, null, null, DialogButtonConditionMode.LOCK);

  public DialogOptionsData(
      boolean allowEscClose,
      boolean showCloseButton,
      boolean displayAvatar,
      Integer avatarTop,
      Integer avatarLeft,
      Integer avatarScale) {
    this(
        allowEscClose,
        showCloseButton,
        displayAvatar,
        avatarTop,
        avatarLeft,
        avatarScale,
        DialogOptionsConfig.BUTTON_CONDITION_MODE);
  }

  public DialogOptionsData() {
    this(
        DialogOptionsConfig.ALLOW_ESC_CLOSE,
        DialogOptionsConfig.SHOW_CLOSE_BUTTON,
        DialogOptionsConfig.DISPLAY_AVATAR,
        null,
        null,
        null,
        DialogOptionsConfig.BUTTON_CONDITION_MODE);
  }

  public static DialogOptionsData getDefault() {
    return new DialogOptionsData(
        DialogOptionsConfig.ALLOW_ESC_CLOSE,
        DialogOptionsConfig.SHOW_CLOSE_BUTTON,
        DialogOptionsConfig.DISPLAY_AVATAR,
        null,
        null,
        null,
        DialogOptionsConfig.BUTTON_CONDITION_MODE);
  }

  public static DialogOptionsData load(CompoundTag compoundTag) {
    boolean allowEscClose =
        compoundTag.contains(DATA_ALLOW_ESC_CLOSE_TAG)
            ? compoundTag.getBoolean(DATA_ALLOW_ESC_CLOSE_TAG)
            : DialogOptionsConfig.ALLOW_ESC_CLOSE;
    boolean showCloseButton =
        compoundTag.contains(DATA_SHOW_CLOSE_BUTTON_TAG)
            ? compoundTag.getBoolean(DATA_SHOW_CLOSE_BUTTON_TAG)
            : DialogOptionsConfig.SHOW_CLOSE_BUTTON;
    boolean displayAvatar =
        compoundTag.contains(DATA_DISPLAY_AVATAR_TAG)
            ? compoundTag.getBoolean(DATA_DISPLAY_AVATAR_TAG)
            : DialogOptionsConfig.DISPLAY_AVATAR;
    Integer avatarTop =
        compoundTag.contains(DATA_AVATAR_TOP_TAG) ? compoundTag.getInt(DATA_AVATAR_TOP_TAG) : null;
    Integer avatarLeft =
        compoundTag.contains(DATA_AVATAR_LEFT_TAG)
            ? compoundTag.getInt(DATA_AVATAR_LEFT_TAG)
            : null;
    Integer avatarScale =
        compoundTag.contains(DATA_AVATAR_SCALE_TAG)
            ? compoundTag.getInt(DATA_AVATAR_SCALE_TAG)
            : null;
    DialogButtonConditionMode buttonConditionMode =
        compoundTag.contains(DATA_BUTTON_CONDITION_MODE_TAG)
            ? DialogButtonConditionMode.get(compoundTag.getString(DATA_BUTTON_CONDITION_MODE_TAG))
            : DialogOptionsConfig.BUTTON_CONDITION_MODE;

    return new DialogOptionsData(
        allowEscClose,
        showCloseButton,
        displayAvatar,
        avatarTop,
        avatarLeft,
        avatarScale,
        buttonConditionMode);
  }

  public boolean hasAvatarTop() {
    return this.avatarTop != null;
  }

  public boolean hasAvatarLeft() {
    return this.avatarLeft != null;
  }

  public boolean hasAvatarScale() {
    return this.avatarScale != null;
  }

  public boolean isDefault() {
    return this.allowEscClose == DialogOptionsConfig.ALLOW_ESC_CLOSE
        && this.showCloseButton == DialogOptionsConfig.SHOW_CLOSE_BUTTON
        && this.displayAvatar == DialogOptionsConfig.DISPLAY_AVATAR
        && this.avatarTop == null
        && this.avatarLeft == null
        && this.avatarScale == null
        && this.buttonConditionMode == DialogOptionsConfig.BUTTON_CONDITION_MODE;
  }

  public CompoundTag save(CompoundTag compoundTag) {
    if (this.allowEscClose != DialogOptionsConfig.ALLOW_ESC_CLOSE) {
      compoundTag.putBoolean(DATA_ALLOW_ESC_CLOSE_TAG, this.allowEscClose);
    }
    if (this.showCloseButton != DialogOptionsConfig.SHOW_CLOSE_BUTTON) {
      compoundTag.putBoolean(DATA_SHOW_CLOSE_BUTTON_TAG, this.showCloseButton);
    }
    if (this.displayAvatar != DialogOptionsConfig.DISPLAY_AVATAR) {
      compoundTag.putBoolean(DATA_DISPLAY_AVATAR_TAG, this.displayAvatar);
    }
    if (this.avatarTop != null) {
      compoundTag.putInt(DATA_AVATAR_TOP_TAG, this.avatarTop);
    }
    if (this.avatarLeft != null) {
      compoundTag.putInt(DATA_AVATAR_LEFT_TAG, this.avatarLeft);
    }
    if (this.avatarScale != null) {
      compoundTag.putInt(DATA_AVATAR_SCALE_TAG, this.avatarScale);
    }
    if (this.buttonConditionMode != DialogOptionsConfig.BUTTON_CONDITION_MODE) {
      compoundTag.putString(DATA_BUTTON_CONDITION_MODE_TAG, this.buttonConditionMode.name());
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }
}
