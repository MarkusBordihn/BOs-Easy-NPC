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

package de.markusbordihn.easynpc.data.display;

import de.markusbordihn.easynpc.Constants;
import java.util.HashSet;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DisplayAttributeSet {

  public static final StreamCodec<RegistryFriendlyByteBuf, DisplayAttributeSet> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public DisplayAttributeSet decode(RegistryFriendlyByteBuf registryFriendlyByteBuf) {
          return new DisplayAttributeSet(registryFriendlyByteBuf.readNbt());
        }

        @Override
        public void encode(
            RegistryFriendlyByteBuf registryFriendlyByteBuf, DisplayAttributeSet objectiveDataSet) {
          registryFriendlyByteBuf.writeNbt(objectiveDataSet.createTag());
        }
      };
  public static final String DATA_DISPLAY_ATTRIBUTE_SET_TAG = "DisplayAttributeSet";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private final HashSet<DisplayAttributeEntry> displayAttributeEntrySet = new HashSet<>();

  public DisplayAttributeSet() {
    this.addDisplayAttribute(DisplayAttributeType.LIGHT_LEVEL, 7);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_AT_DAY, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_AT_NIGHT, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_IN_CREATIVE, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_IN_SPECTATOR, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_IN_STANDARD, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_TO_OWNER, true);
    this.addDisplayAttribute(DisplayAttributeType.VISIBLE_TO_TEAM, true);
  }

  public DisplayAttributeSet(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public void addDisplayAttribute(
      DisplayAttributeType displayAttributeType, boolean booleanValue, int intValue) {
    displayAttributeEntrySet.add(
        new DisplayAttributeEntry(displayAttributeType, booleanValue, intValue));
  }

  public void addOrReplaceDisplayAttribute(DisplayAttributeEntry displayAttributeEntry) {
    displayAttributeEntrySet.removeIf(
        entry -> entry.displayAttributeType().equals(displayAttributeEntry.displayAttributeType()));
    displayAttributeEntrySet.add(displayAttributeEntry);
  }

  public void addDisplayAttribute(DisplayAttributeType displayAttributeType, boolean booleanValue) {
    this.addDisplayAttribute(displayAttributeType, booleanValue, 0);
  }

  public void addDisplayAttribute(DisplayAttributeType displayAttributeType, int intValue) {
    this.addDisplayAttribute(displayAttributeType, false, intValue);
  }

  public boolean hasDisplayAttribute(DisplayAttributeType displayAttributeType) {
    return displayAttributeEntrySet.stream()
        .anyMatch(entry -> entry.displayAttributeType().equals(displayAttributeType));
  }

  public boolean getBooleanValue(DisplayAttributeType displayAttributeType) {
    return displayAttributeEntrySet.stream()
        .filter(entry -> entry.displayAttributeType().equals(displayAttributeType))
        .map(DisplayAttributeEntry::booleanValue)
        .findFirst()
        .orElse(false);
  }

  public int getIntValue(DisplayAttributeType displayAttributeType) {
    return displayAttributeEntrySet.stream()
        .filter(entry -> entry.displayAttributeType().equals(displayAttributeType))
        .map(DisplayAttributeEntry::intValue)
        .findFirst()
        .orElse(0);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    ListTag displayListTag = new ListTag();
    displayAttributeEntrySet.stream()
        .filter(entry -> entry.displayAttributeType() != DisplayAttributeType.NONE)
        .forEach(entry -> displayListTag.add(entry.write(new CompoundTag())));
    compoundTag.put(DATA_DISPLAY_ATTRIBUTE_SET_TAG, displayListTag);
    return compoundTag;
  }

  public void load(CompoundTag compoundTag) {
    if (compoundTag == null || !compoundTag.contains(DATA_DISPLAY_ATTRIBUTE_SET_TAG)) {
      return;
    }

    ListTag displayListTag = compoundTag.getList(DATA_DISPLAY_ATTRIBUTE_SET_TAG, 10);
    displayListTag.forEach(
        entry -> {
          if (entry instanceof CompoundTag entryCompoundTag) {
            DisplayAttributeEntry displayAttributeEntry =
                new DisplayAttributeEntry(entryCompoundTag);
            if (displayAttributeEntry.displayAttributeType() != DisplayAttributeType.NONE) {
              displayAttributeEntrySet.add(displayAttributeEntry);
            } else {
              log.warn("Skip invalid display attribute entry {}", displayAttributeEntry);
            }
          } else {
            log.error("Failed to load display attribute entry from {}", entry);
          }
        });
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DisplayDataSet{" + "displayAttributeSet=" + displayAttributeEntrySet + '}';
  }
}
