/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.data.attribute;

import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import java.util.Set;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class LegacyAttributeConverter {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final String LEGACY_ATTRIBUTES_TAG = "Attributes";
  private static final String LEGACY_NAME_TAG = "Name";
  private static final String LEGACY_BASE_TAG = "Base";
  private static final String ATTRIBUTES_TAG = "attributes";
  private static final String ID_TAG = "id";
  private static final String BASE_TAG = "base";

  private static final String MINECRAFT_NAMESPACE = "minecraft:";

  // Vanilla attributes lost their group prefixes in 1.21 (e.g. "minecraft:generic.max_health"
  // became "minecraft:max_health"). Strip these so the ids resolve against the modern registry.
  private static final Set<String> LEGACY_GROUP_PREFIXES =
      Set.of("generic.", "horse.", "zombie.", "player.");

  // Legacy Forge-specific attributes that were merged into vanilla in 1.21.
  private static final Map<String, String> LEGACY_FORGE_ATTRIBUTES =
      Map.of(
          "forge:entity_gravity", "minecraft:gravity",
          "forge:step_height_addition", "minecraft:step_height");

  private LegacyAttributeConverter() {}

  static String convertAttributeId(String legacyName) {
    if (legacyName == null || legacyName.isEmpty()) {
      return legacyName;
    }
    if (legacyName.startsWith(MINECRAFT_NAMESPACE)) {
      String path = legacyName.substring(MINECRAFT_NAMESPACE.length());
      for (String prefix : LEGACY_GROUP_PREFIXES) {
        if (path.startsWith(prefix)) {
          return MINECRAFT_NAMESPACE + path.substring(prefix.length());
        }
      }
      return legacyName;
    }
    return LEGACY_FORGE_ATTRIBUTES.getOrDefault(legacyName, legacyName);
  }

  public static boolean convertLegacyAttributes(CompoundTag compoundTag) {
    if (compoundTag == null) {
      return false;
    }

    // Data already using the 1.21 structure may still carry pre-1.21 attribute ids (e.g. when it
    // was restructured by an earlier converter that did not rename them). Normalize those in place.
    if (compoundTag.contains(ATTRIBUTES_TAG)) {
      return normalizeModernAttributeIds(compoundTag.getListOrEmpty(ATTRIBUTES_TAG));
    }

    if (!compoundTag.contains(LEGACY_ATTRIBUTES_TAG)) {
      return false;
    }

    ListTag legacyList = compoundTag.getListOrEmpty(LEGACY_ATTRIBUTES_TAG);
    ListTag convertedList = new ListTag();
    for (int index = 0; index < legacyList.size(); index++) {
      CompoundTag legacyEntry = legacyList.getCompound(index).orElse(new CompoundTag());
      if (!legacyEntry.contains(LEGACY_NAME_TAG)) {
        continue;
      }
      CompoundTag convertedEntry = new CompoundTag();
      convertedEntry.putString(
          ID_TAG, convertAttributeId(legacyEntry.getString(LEGACY_NAME_TAG).orElse("")));
      if (legacyEntry.contains(LEGACY_BASE_TAG)) {
        convertedEntry.putDouble(BASE_TAG, legacyEntry.getDouble(LEGACY_BASE_TAG).orElse(0.0));
      }
      convertedList.add(convertedEntry);
    }

    compoundTag.remove(LEGACY_ATTRIBUTES_TAG);
    compoundTag.put(ATTRIBUTES_TAG, convertedList);
    log.info("Converted {} legacy entity attribute(s) to 1.21 format.", convertedList.size());
    return true;
  }

  private static boolean normalizeModernAttributeIds(ListTag attributes) {
    int updated = 0;
    for (int index = 0; index < attributes.size(); index++) {
      CompoundTag entry = attributes.getCompound(index).orElse(new CompoundTag());
      String currentId = entry.getString(ID_TAG).orElse("");
      String convertedId = convertAttributeId(currentId);
      if (!convertedId.equals(currentId)) {
        entry.putString(ID_TAG, convertedId);
        updated++;
      }
    }
    if (updated > 0) {
      log.info("Normalized {} pre-1.21 entity attribute id(s) to 1.21 format.", updated);
    }
    return updated > 0;
  }
}
