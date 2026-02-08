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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.data.display.DisplayAttributeDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeEntry;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Mob;

public interface DisplayAttributeDataCapable<E extends Mob> extends EasyNPC<E> {

  String DATA_DISPLAY_ATTRIBUTE_TAG = "DisplayAttribute";

  StreamCodec<RegistryFriendlyByteBuf, EnumMap<DisplayAttributeType, DisplayAttributeEntry>>
      STREAM_CODEC =
          new StreamCodec<>() {
            @Override
            public EnumMap<DisplayAttributeType, DisplayAttributeEntry> decode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf) {
              CompoundTag compoundTag = registryFriendlyByteBuf.readNbt();
              EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
                  new EnumMap<>(DisplayAttributeType.class);
              for (String key : compoundTag.getAllKeys()) {
                DisplayAttributeType displayAttributeType = DisplayAttributeType.get(key);
                if (displayAttributeType != null) {
                  displayAttributeMap.put(
                      displayAttributeType,
                      new DisplayAttributeEntry(compoundTag.getCompound(key)));
                }
              }
              return displayAttributeMap;
            }

            @Override
            public void encode(
                RegistryFriendlyByteBuf registryFriendlyByteBuf,
                EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap) {
              CompoundTag compoundTag = new CompoundTag();
              for (Map.Entry<DisplayAttributeType, DisplayAttributeEntry> entry :
                  displayAttributeMap.entrySet()) {
                CompoundTag entryTag = new CompoundTag();
                entry.getValue().write(entryTag);
                compoundTag.put(entry.getKey().name(), entryTag);
              }
              registryFriendlyByteBuf.writeNbt(compoundTag);
            }
          };

  default DisplayAttributeDataSet getDisplayAttributeData() {
    DisplayAttributeDataSet displayAttributeDataSet =
        getSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET);
    if (displayAttributeDataSet == null) {
      displayAttributeDataSet = DisplayAttributeDataSet.createDefault();
      setDisplayAttributeData(displayAttributeDataSet);
    }
    return displayAttributeDataSet;
  }

  default void setDisplayAttributeData(DisplayAttributeDataSet displayAttributeDataSet) {
    if (displayAttributeDataSet != null) {
      setSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, displayAttributeDataSet, true);
      syncDisplayAttributesToEntity(displayAttributeDataSet);
    }
  }

  default void syncDisplayAttributesToEntity(DisplayAttributeDataSet displayAttributeDataSet) {

    // Sync customNameVisible property from NAME_VISIBILITY attribute
    if (displayAttributeDataSet.hasAttribute(DisplayAttributeType.NAME_VISIBILITY)) {
      DisplayAttributeEntry nameVisibilityEntry =
          displayAttributeDataSet.getAttribute(DisplayAttributeType.NAME_VISIBILITY);
      if (nameVisibilityEntry != null) {
        try {
          NameVisibilityType nameVisibilityType =
              NameVisibilityType.valueOf(nameVisibilityEntry.stringValue());
          getEntity().setCustomNameVisible(nameVisibilityType != NameVisibilityType.NEVER);
        } catch (IllegalArgumentException e) {
          log.warn("Invalid name visibility type: {}", nameVisibilityEntry.stringValue());
        }
      }
    }
  }

  default void clearDisplayAttributeData() {
    setDisplayAttributeData(DisplayAttributeDataSet.createDefault());
  }

  default boolean hasDisplayAttribute(DisplayAttributeType displayAttributeType) {
    return getDisplayAttributeData().hasAttribute(displayAttributeType);
  }

  default boolean getDisplayBooleanAttribute(DisplayAttributeType displayAttributeType) {
    DisplayAttributeEntry entry = getDisplayAttributeData().getAttribute(displayAttributeType);
    return entry != null && entry.booleanValue();
  }

  default int getDisplayIntAttribute(DisplayAttributeType displayAttributeType) {
    DisplayAttributeEntry entry = getDisplayAttributeData().getAttribute(displayAttributeType);
    return entry != null ? entry.intValue() : 0;
  }

  default String getDisplayStringAttribute(DisplayAttributeType displayAttributeType) {
    DisplayAttributeEntry entry = getDisplayAttributeData().getAttribute(displayAttributeType);
    return entry != null ? entry.stringValue() : "";
  }

  default <T extends Enum<T>> T getDisplayEnumAttribute(
      DisplayAttributeType displayAttributeType, Class<T> enumClass) {
    String enumValue = getDisplayStringAttribute(displayAttributeType);
    try {
      return Enum.valueOf(enumClass, enumValue);
    } catch (IllegalArgumentException e) {
      log.error("Failed to get enum value {} for class {}: {}", enumValue, enumClass, e);
    }
    return null;
  }

  default <T> void setDisplayAttribute(
      DisplayAttributeType displayAttributeType, ValueType valueType, T value) {
    DisplayAttributeEntry newEntry =
        switch (valueType) {
          case BOOLEAN -> new DisplayAttributeEntry((boolean) value);
          case INTEGER -> new DisplayAttributeEntry((int) value);
          case STRING -> new DisplayAttributeEntry((String) value);
          default -> {
            log.error("Invalid display value type {} for {}", valueType, displayAttributeType);
            yield null;
          }
        };

    if (newEntry != null) {
      setDisplayAttributeData(
          getDisplayAttributeData().withAttribute(displayAttributeType, newEntry));
    }
  }

  default void setDisplayAttribute(DisplayAttributeType displayAttributeType, Enum<?> enumValue) {
    setDisplayAttribute(displayAttributeType, ValueType.STRING, enumValue.toString());
  }

  default void defineSynchedDisplayAttributeData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, DisplayAttributeDataSet.createDefault());
  }

  default void readAdditionalDisplayAttributeData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_DISPLAY_ATTRIBUTE_TAG)) {
      return;
    }

    DisplayAttributeDataSet displayAttributeData =
        new DisplayAttributeDataSet(compoundTag.getList(DATA_DISPLAY_ATTRIBUTE_TAG, 10));
    setDisplayAttributeData(displayAttributeData);
  }

  default void addAdditionalDisplayAttributeData(CompoundTag compoundTag) {
    DisplayAttributeDataSet displayAttributeData = getDisplayAttributeData();
    if (displayAttributeData != null) {
      compoundTag.put(DATA_DISPLAY_ATTRIBUTE_TAG, displayAttributeData.save());
    }
  }
}
