/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.npc;

import de.markusbordihn.easynpc.utils.EnumUtils;
import java.util.Locale;

public enum RawNPCType implements NPCType {
  GENERIC,
  // ===== AUTO_GENERATED_START: Do not edit between these markers =====
  ALLAY,
  BOGGED,
  CAT,
  CREEPER,
  CHICKEN,
  DROWNED,
  ENDERMAN,
  EVOKER,
  FOX,
  GHAST,
  HORSE,
  IRON_GOLEM,
  ILLUSIONER,
  PATHFINDER_MOB,
  HUMANOID,
  HUMANOID_SLIM,
  PILLAGER,
  PIG,
  PIGLIN,
  PIGLIN_BRUTE,
  ZOMBIFIED_PIGLIN,
  SKELETON,
  STRAY,
  WITHER_SKELETON,
  SPIDER,
  SLIME,
  VEX,
  VILLAGER,
  WANDERING_TRADER,
  VINDICATOR,
  WOLF,
  WITCH,
  ZOMBIE,
  HUSK,
  ZOMBIE_VILLAGER;
  // ===== AUTO_GENERATED_END =====

  private static final String REGISTRY_ID_SUFFIX = "_raw";

  private final String registryId = this.name().toLowerCase(Locale.ROOT) + REGISTRY_ID_SUFFIX;

  public static RawNPCType fromRegistryId(String registryId) {
    if (registryId == null || !registryId.endsWith(REGISTRY_ID_SUFFIX)) {
      return null;
    }

    return EnumUtils.getIgnoreCase(
        RawNPCType.class,
        registryId.substring(0, registryId.length() - REGISTRY_ID_SUFFIX.length()),
        null);
  }

  @Override
  public String getRegistryId() {
    return this.registryId;
  }
}
