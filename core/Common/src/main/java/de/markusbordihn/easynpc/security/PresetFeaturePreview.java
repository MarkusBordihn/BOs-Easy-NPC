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

package de.markusbordihn.easynpc.security;

import java.util.ArrayList;
import java.util.List;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

public record PresetFeaturePreview(List<PresetFeatureNotice> notices) {

  public static final String PREVIEW_TAG = "PresetFeaturePreview";
  public static final String NOTICES_TAG = "Notices";

  public static PresetFeaturePreview empty() {
    return new PresetFeaturePreview(List.of());
  }

  public static PresetFeaturePreview fromTag(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(PREVIEW_TAG)) {
      return empty();
    }

    List<PresetFeatureNotice> notices = new ArrayList<>();
    ListTag listTag = compoundTag.getCompound(PREVIEW_TAG).getList(NOTICES_TAG, Tag.TAG_COMPOUND);
    for (int i = 0; i < listTag.size(); i++) {
      PresetFeatureNotice notice = PresetFeatureNotice.fromTag(listTag.getCompound(i));
      if (notice != null) {
        notices.add(notice);
      }
    }

    return new PresetFeaturePreview(List.copyOf(notices));
  }

  public boolean hasNotices() {
    return this.notices != null && !this.notices.isEmpty();
  }

  public CompoundTag toTag() {
    ListTag listTag = new ListTag();
    if (this.notices != null) {
      for (PresetFeatureNotice notice : this.notices) {
        if (notice != null) {
          listTag.add(notice.toTag());
        }
      }
    }

    CompoundTag previewTag = new CompoundTag();
    previewTag.put(NOTICES_TAG, listTag);
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.put(PREVIEW_TAG, previewTag);

    return compoundTag;
  }
}
