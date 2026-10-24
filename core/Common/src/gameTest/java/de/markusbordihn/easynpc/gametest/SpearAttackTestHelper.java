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

import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.item.ModItemTags;
import java.util.List;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.tags.TagKey;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.PiercingWeapon;
import net.minecraft.world.phys.Vec3;

public class SpearAttackTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);
  private static final TagKey<Item> SPEARS =
      TagKey.create(Registries.ITEM, Identifier.withDefaultNamespace("spears"));

  private SpearAttackTestHelper() {}

  public static void assertSpearsAreInPassInteractionTag(GameTestHelper helper) {
    List<Item> spears =
        BuiltInRegistries.ITEM.stream().filter(item -> new ItemStack(item).is(SPEARS)).toList();
    GameTestHelpers.assertTrue(
        helper, "The minecraft:spears item tag must not be empty", !spears.isEmpty());
    for (Item spear : spears) {
      GameTestHelpers.assertTrue(
          helper,
          spear + " must be in the pass_interaction item tag",
          new ItemStack(spear).is(ModItemTags.PASS_INTERACTION));
    }
  }

  public static void assertAttackableNPCCanBeHitBySpear(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setCombatAttribute(
        easyNPC, CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS, true);
    AttributeHandler.setCombatAttribute(easyNPC, CombatAttributeType.IS_INVULNERABLE, false);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, "spear-attackable");

    GameTestHelpers.assertTrue(
        helper,
        "The NPC must reject projectiles, otherwise vanilla alone would allow the spear hit",
        !easyNPC.getLivingEntity().canBeHitByProjectile());
    GameTestHelpers.assertTrue(
        helper,
        "A spear must hit an NPC that is attackable by players",
        PiercingWeapon.canHitEntity(serverPlayer, easyNPC.getLivingEntity()));
  }

  public static void assertInvulnerableNPCCannotBeHitBySpear(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setCombatAttribute(
        easyNPC, CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS, true);
    AttributeHandler.setCombatAttribute(easyNPC, CombatAttributeType.IS_INVULNERABLE, true);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, "spear-invulnerable");

    GameTestHelpers.assertTrue(
        helper,
        "A spear must not hit an invulnerable NPC",
        !PiercingWeapon.canHitEntity(serverPlayer, easyNPC.getLivingEntity()));
  }
}
