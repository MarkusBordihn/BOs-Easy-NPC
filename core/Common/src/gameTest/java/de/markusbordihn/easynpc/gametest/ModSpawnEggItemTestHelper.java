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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.item.ModSpawnEggItem;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.SpawnEggItem;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;

public final class ModSpawnEggItemTestHelper {

  private static final BlockPos SPAWN_POSITION = new BlockPos(1, 2, 1);

  // Large NPCs like the Ghast are pushed away from the clicked block to fit their hitbox, so the
  // spawned entity is only checked within this radius instead of on the exact block.
  private static final double SPAWN_SEARCH_RADIUS = 4.0D;

  private ModSpawnEggItemTestHelper() {}

  public static void useAllSpawnEggItems(GameTestHelper helper) {
    List<String> errors = new ArrayList<>();
    for (SpawnEggItem spawnEggItem : getAllSpawnEggItems(helper)) {
      ResourceLocation itemId = BuiltInRegistries.ITEM.getKey(spawnEggItem);
      EntityType<?> expectedEntityType = getExpectedEntityType(itemId);
      if (expectedEntityType == null) {
        errors.add(itemId + " has no matching entity type");
        continue;
      }

      EntityType<?> entityType = spawnEggItem.getType(new ItemStack(spawnEggItem).getTag());
      if (entityType != expectedEntityType) {
        errors.add(itemId + " has entity type " + entityType);
      }
      if (SpawnEggItem.byId(expectedEntityType) != spawnEggItem) {
        errors.add(itemId + " is not found by SpawnEggItem.byId");
      }
      if (!spawnsEntity(helper, spawnEggItem, expectedEntityType)) {
        errors.add(itemId + " does not spawn " + EntityType.getKey(expectedEntityType));
      }
    }
    if (!errors.isEmpty()) {
      helper.fail(errors.size() + " spawn egg checks failed: " + String.join(", ", errors));
    }
  }

  private static boolean spawnsEntity(
      GameTestHelper helper, SpawnEggItem spawnEggItem, EntityType<?> entityType) {
    Player player = helper.makeMockPlayer();
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(spawnEggItem));
    spawnEggItem.useOn(
        new UseOnContext(
            player,
            InteractionHand.MAIN_HAND,
            new BlockHitResult(
                helper.absoluteVec(new Vec3(1, 2, 1)),
                Direction.UP,
                helper.absolutePos(SPAWN_POSITION),
                false)));
    AABB searchArea = new AABB(helper.absolutePos(SPAWN_POSITION)).inflate(SPAWN_SEARCH_RADIUS);
    return !helper.getLevel().getEntities(entityType, searchArea, Entity::isAlive).isEmpty();
  }

  private static List<SpawnEggItem> getAllSpawnEggItems(GameTestHelper helper) {
    List<SpawnEggItem> spawnEggItems = new ArrayList<>();
    for (Item item : BuiltInRegistries.ITEM) {
      ResourceLocation itemId = BuiltInRegistries.ITEM.getKey(item);
      if (!Constants.MOD_ID.equals(itemId.getNamespace())
          || !itemId.getPath().endsWith(ModSpawnEggItem.SUFFIX)) {
        continue;
      }

      if (item instanceof SpawnEggItem spawnEggItem) {
        spawnEggItems.add(spawnEggItem);
      } else {
        helper.fail(itemId + " is not a SpawnEggItem!");
      }
    }
    if (spawnEggItems.isEmpty()) {
      helper.fail("No spawn egg items registered for " + Constants.MOD_ID + "!");
    }
    return spawnEggItems;
  }

  private static EntityType<?> getExpectedEntityType(ResourceLocation spawnEggItemId) {
    String itemPath = spawnEggItemId.getPath();
    return BuiltInRegistries.ENTITY_TYPE
        .getOptional(
            spawnEggItemId.withPath(
                itemPath.substring(0, itemPath.length() - ModSpawnEggItem.SUFFIX.length())))
        .orElse(null);
  }
}
