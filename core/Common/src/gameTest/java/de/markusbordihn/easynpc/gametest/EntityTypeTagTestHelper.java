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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.handlers.SmartAttackHandler;
import java.util.List;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.entity.EntityType;

public class EntityTypeTagTestHelper {

  private static final List<EntityType<?>> RANGED_TARGETS =
      List.of(
          EntityType.BLAZE,
          EntityType.CREEPER,
          EntityType.GHAST,
          EntityType.PHANTOM,
          EntityType.VEX);
  private static final List<String> EMPTY_EASY_NPC_TAGS =
      List.of("illagers", "skeletons", "zombies");

  private EntityTypeTagTestHelper() {}

  public static void assertEntityTypeTagsContainExpectedEntries(GameTestHelper helper) {
    for (String tagPath : EMPTY_EASY_NPC_TAGS) {
      TagKey<EntityType<?>> tagKey =
          TagKey.create(Registries.ENTITY_TYPE, new ResourceLocation(Constants.MOD_ID, tagPath));
      GameTestHelpers.assertTrue(
          helper,
          "The tag " + tagKey.location() + " must be loaded",
          BuiltInRegistries.ENTITY_TYPE.getTag(tagKey).isPresent());
    }
    for (EntityType<?> entityType : RANGED_TARGETS) {
      GameTestHelpers.assertTrue(
          helper,
          entityType + " must be a smart attack ranged target",
          entityType.is(SmartAttackHandler.RANGED_TARGET));
    }
    GameTestHelpers.assertTrue(
        helper,
        "A zombie must not be a smart attack ranged target",
        !EntityType.ZOMBIE.is(SmartAttackHandler.RANGED_TARGET));
  }
}
