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

import de.markusbordihn.easynpc.data.display.DisplayAttributeEntry;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;

public interface DisplayAttributeDataCapable<E extends PathfinderMob> extends EasyNPC<E> {

  String DATA_DISPLAY_ATTRIBUTE_SET_TAG = "DisplayAttribute";

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

  default EnumMap<DisplayAttributeType, DisplayAttributeEntry> getDisplayAttributeMap() {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET);
    if (displayAttributeMap == null) {
      displayAttributeMap = createDefaultDisplayAttributeMap();
      setDisplayAttributeMap(displayAttributeMap);
    }
    return displayAttributeMap;
  }

  default void setDisplayAttributeMap(
      EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap) {
    if (displayAttributeMap != null) {
      setSynchedEntityData(SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, displayAttributeMap, true);
    }
  }

  default void clearDisplayAttributeMap() {
    setSynchedEntityData(
        SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, createDefaultDisplayAttributeMap());
  }

  default void updateDisplayAttributeMap() {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    if (displayAttributeMap != null) {
      this.setDisplayAttributeMap(new EnumMap<>(displayAttributeMap));
    }
  }

  default boolean hasDisplayAttribute(DisplayAttributeType displayAttributeType) {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    return displayAttributeMap.containsKey(displayAttributeType);
  }

  default boolean getDisplayBooleanAttribute(DisplayAttributeType displayAttributeType) {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    DisplayAttributeEntry entry = displayAttributeMap.get(displayAttributeType);
    return entry != null ? entry.booleanValue() : false;
  }

  default int getDisplayIntAttribute(DisplayAttributeType displayAttributeType) {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    DisplayAttributeEntry entry = displayAttributeMap.get(displayAttributeType);
    return entry != null ? entry.intValue() : 0;
  }

  default String getDisplayStringAttribute(DisplayAttributeType displayAttributeType) {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    DisplayAttributeEntry entry = displayAttributeMap.get(displayAttributeType);
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
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    switch (valueType) {
      case BOOLEAN ->
          displayAttributeMap.put(displayAttributeType, new DisplayAttributeEntry((boolean) value));
      case INTEGER ->
          displayAttributeMap.put(displayAttributeType, new DisplayAttributeEntry((int) value));
      case STRING ->
          displayAttributeMap.put(displayAttributeType, new DisplayAttributeEntry((String) value));
      default -> {
        log.error("Invalid display value type {} for {}", valueType, displayAttributeType);
        return;
      }
    }
    this.updateDisplayAttributeMap();
  }

  default void setDisplayAttribute(DisplayAttributeType displayAttributeType, Enum<?> enumValue) {
    setDisplayAttribute(displayAttributeType, ValueType.STRING, enumValue.toString());
  }

  default EnumMap<DisplayAttributeType, DisplayAttributeEntry> createDefaultDisplayAttributeMap() {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> map =
        new EnumMap<>(DisplayAttributeType.class);
    map.put(DisplayAttributeType.LIGHT_LEVEL, new DisplayAttributeEntry(7));
    map.put(DisplayAttributeType.VISIBLE, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_AT_DAY, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_AT_NIGHT, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_IN_CREATIVE, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_IN_SPECTATOR, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_IN_STANDARD, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_TO_OWNER, new DisplayAttributeEntry(true));
    map.put(DisplayAttributeType.VISIBLE_TO_TEAM, new DisplayAttributeEntry(true));
    map.put(
        DisplayAttributeType.NAME_VISIBILITY,
        new DisplayAttributeEntry(NameVisibilityType.ALWAYS.toString()));
    return map;
  }

  default void defineSynchedDisplayAttributeData(SynchedEntityData.Builder builder) {
    defineSynchedEntityData(
        builder, SynchedDataIndex.DISPLAY_ATTRIBUTE_SET, createDefaultDisplayAttributeMap());
  }

  default void readAdditionalDisplayAttributeData(CompoundTag compoundTag) {
    if (!compoundTag.contains(DATA_DISPLAY_ATTRIBUTE_SET_TAG)) {
      return;
    }

    ListTag displayListTag = compoundTag.getList(DATA_DISPLAY_ATTRIBUTE_SET_TAG, 10);
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        new EnumMap<>(DisplayAttributeType.class);

    displayListTag.forEach(
        entry -> {
          if (entry instanceof CompoundTag entryCompoundTag) {
            if (entryCompoundTag.contains("Type")) {
              String typeString = entryCompoundTag.getString("Type");
              DisplayAttributeType displayAttributeType = DisplayAttributeType.get(typeString);
              if (displayAttributeType != DisplayAttributeType.NONE) {
                DisplayAttributeEntry displayAttributeEntry =
                    new DisplayAttributeEntry(entryCompoundTag);
                displayAttributeMap.put(displayAttributeType, displayAttributeEntry);
              } else {
                log.warn("Skip invalid display attribute type {}", typeString);
              }
            } else {
              log.warn("Skip display attribute entry without type information");
            }
          } else {
            log.error("Failed to load display attribute entry from {}", entry);
          }
        });

    setDisplayAttributeMap(displayAttributeMap);
  }

  default void addAdditionalDisplayAttributeData(CompoundTag compoundTag) {
    EnumMap<DisplayAttributeType, DisplayAttributeEntry> displayAttributeMap =
        getDisplayAttributeMap();
    if (displayAttributeMap != null && !displayAttributeMap.isEmpty()) {
      ListTag displayListTag = new ListTag();
      displayAttributeMap.entrySet().stream()
          .filter(mapEntry -> mapEntry.getKey() != DisplayAttributeType.NONE)
          .forEach(
              mapEntry -> {
                CompoundTag entryTag = new CompoundTag();
                entryTag.putString("Type", mapEntry.getKey().name());
                mapEntry.getValue().write(entryTag);
                displayListTag.add(entryTag);
              });
      compoundTag.put(DATA_DISPLAY_ATTRIBUTE_SET_TAG, displayListTag);
    }
  }
}
