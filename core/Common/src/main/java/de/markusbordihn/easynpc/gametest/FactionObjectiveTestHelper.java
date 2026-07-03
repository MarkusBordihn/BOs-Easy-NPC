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
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import de.markusbordihn.easynpc.handler.FactionHandler;
import de.markusbordihn.easynpc.handler.ObjectiveHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.damagesource.DamageSource;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.scores.PlayerTeam;
import net.minecraft.world.scores.Scoreboard;

public class FactionObjectiveTestHelper {

  private FactionObjectiveTestHelper() {}

  public static void assertFactionScoreboardMembership(
      GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> redNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    EasyNPC<?> blueNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);

    GameTestHelpers.assertTrue(
        helper, "Failed to set red faction", FactionHandler.setFaction(redNPC, "red"));
    GameTestHelpers.assertTrue(
        helper, "Failed to set blue faction", FactionHandler.setFaction(blueNPC, "blue"));

    GameTestHelpers.assertTrue(
        helper, "Faction 'red' missing in registry", FactionData.get().hasFaction("red"));
    GameTestHelpers.assertTrue(
        helper, "Faction 'blue' missing in registry", FactionData.get().hasFaction("blue"));

    GameTestHelpers.assertNotNull(
        helper, "Red NPC has no scoreboard team", redNPC.getEntity().getTeam());
    GameTestHelpers.assertEquals(
        helper, "Red NPC is in wrong team", "red", redNPC.getEntity().getTeam().getName());
    GameTestHelpers.assertEquals(
        helper, "Blue NPC is in wrong team", "blue", blueNPC.getEntity().getTeam().getName());

    GameTestHelpers.assertTrue(
        helper, "Failed to remove faction", FactionHandler.removeFaction(redNPC));
    GameTestHelpers.assertTrue(
        helper,
        "Red NPC should no longer have a scoreboard team",
        redNPC.getEntity().getTeam() == null);
  }

  public static void assertDirectedHostileFactionTargeting(
      GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> redNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    EasyNPC<?> blueNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    FactionHandler.setFaction(redNPC, "red");
    FactionHandler.setFaction(blueNPC, "blue");

    FactionData.get().addHostileFaction("red", "blue");

    GameTestHelpers.assertTrue(
        helper,
        "Red faction should be hostile to blue NPC",
        FactionHandler.isHostile("red", blueNPC.getLivingEntity()));
    GameTestHelpers.assertTrue(
        helper,
        "Blue faction should not be hostile to red NPC (directed hostility)",
        !FactionHandler.isHostile("blue", redNPC.getLivingEntity()));
    GameTestHelpers.assertTrue(
        helper,
        "Red faction should not be hostile to same faction",
        !FactionHandler.isHostile("red", redNPC.getLivingEntity()));

    ObjectiveDataEntry objectiveDataEntry =
        new ObjectiveDataEntry(ObjectiveType.ATTACK_HOSTILE_FACTIONS);
    GameTestHelpers.assertTrue(
        helper,
        "Failed to add attack hostile factions objective",
        ObjectiveHandler.addOrUpdateCustomObjective(redNPC, objectiveDataEntry));
    GameTestHelpers.assertTrue(
        helper,
        "Attack hostile factions objective is not registered",
        redNPC
            .getEasyNPCObjectiveData()
            .getObjectiveDataSet()
            .hasObjective(ObjectiveType.ATTACK_HOSTILE_FACTIONS));
  }

  public static void assertHostileFactionCombatBypass(
      GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> redNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    EasyNPC<?> blueNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    FactionHandler.setFaction(redNPC, "red");
    FactionHandler.setFaction(blueNPC, "blue");
    FactionData.get().addHostileFaction("red", "blue");

    // Without the attackable-by-factions attribute, the invulnerable NPC can not be targeted.
    GameTestHelpers.assertTrue(
        helper,
        "Red NPC should not be able to attack blue NPC without attackable-by-factions",
        !redNPC.getMob().canAttack(blueNPC.getLivingEntity()));

    GameTestHelpers.assertTrue(
        helper,
        "Failed to enable attackable-by-factions for blue NPC",
        AttributeHandler.setCombatAttribute(
            blueNPC, CombatAttributeType.IS_ATTACKABLE_BY_FACTIONS, true));

    GameTestHelpers.assertTrue(
        helper,
        "Red NPC should be able to attack blue NPC despite invulnerability",
        redNPC.getMob().canAttack(blueNPC.getLivingEntity()));
    GameTestHelpers.assertTrue(
        helper,
        "Blue NPC should not be able to attack red NPC (directed hostility)",
        !blueNPC.getMob().canAttack(redNPC.getLivingEntity()));

    DamageSource redAttack = helper.getLevel().damageSources().mobAttack(redNPC.getMob());
    GameTestHelpers.assertTrue(
        helper,
        "Blue NPC should not be invulnerable to red NPC attacks",
        !blueNPC.getLivingEntity().isInvulnerableTo(redAttack));
    DamageSource blueAttack = helper.getLevel().damageSources().mobAttack(blueNPC.getMob());
    GameTestHelpers.assertTrue(
        helper,
        "Red NPC should still be invulnerable to blue NPC attacks",
        redNPC.getLivingEntity().isInvulnerableTo(blueAttack));

    float healthBeforeAttack = blueNPC.getLivingEntity().getHealth();
    GameTestHelpers.assertTrue(
        helper,
        "Blue NPC should take damage from red NPC",
        blueNPC.getLivingEntity().hurt(redAttack, 2.0F));
    GameTestHelpers.assertTrue(
        helper,
        "Blue NPC health should be reduced after the attack",
        blueNPC.getLivingEntity().getHealth() < healthBeforeAttack);
  }

  public static void assertPlayerFactionCombatBypass(
      GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> redNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    EasyNPC<?> blueNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    FactionHandler.setFaction(redNPC, "red");
    FactionHandler.setFaction(blueNPC, "blue");
    FactionData.get().addHostileFaction("red", "blue");
    AttributeHandler.setCombatAttribute(
        blueNPC, CombatAttributeType.IS_ATTACKABLE_BY_FACTIONS, true);

    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    DamageSource playerAttack = helper.getLevel().damageSources().playerAttack(serverPlayer);

    // Without a faction team membership, the player is still blocked by invulnerability.
    GameTestHelpers.assertTrue(
        helper,
        "Player without faction team should not damage the invulnerable NPC",
        blueNPC.getLivingEntity().isInvulnerableTo(playerAttack));

    // Players join a faction through the matching scoreboard team.
    Scoreboard scoreboard = helper.getLevel().getScoreboard();
    PlayerTeam redTeam = scoreboard.getPlayerTeam("red");
    GameTestHelpers.assertNotNull(helper, "Red faction team is missing", redTeam);
    scoreboard.addPlayerToTeam(serverPlayer.getScoreboardName(), redTeam);
    GameTestHelpers.assertTrue(
        helper,
        "Player in hostile faction team should damage the invulnerable NPC",
        !blueNPC.getLivingEntity().isInvulnerableTo(playerAttack));

    // Leaving the team restores the invulnerability protection for the player.
    scoreboard.removePlayerFromTeam(serverPlayer.getScoreboardName(), redTeam);
    GameTestHelpers.assertTrue(
        helper,
        "Player without faction team should be blocked again",
        blueNPC.getLivingEntity().isInvulnerableTo(playerAttack));

    // Attackable by players opens player damage independent of the invulnerability protection.
    AttributeHandler.setCombatAttribute(
        blueNPC, CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS, true);
    GameTestHelpers.assertTrue(
        helper,
        "Attackable by players should allow player damage despite invulnerability",
        !blueNPC.getLivingEntity().isInvulnerableTo(playerAttack));
  }

  public static void assertFactionSurvivesPresetRoundTrip(
      GameTestHelper helper, EntityType<?> entityType) {
    FactionData.init(helper.getLevel().getServer());
    EasyNPC<?> sourceNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    FactionHandler.setFaction(sourceNPC, "guards");

    CompoundTag presetData = sourceNPC.getEasyNPCPresetData().serializePresetData();
    GameTestHelpers.assertTrue(
        helper,
        "Preset data should contain the faction name",
        presetData.contains("FactionName") && "guards".equals(presetData.getString("FactionName")));

    EasyNPC<?> importedNPC = GameTestHelpers.spawnNPCEntityType(helper, entityType);
    presetData.remove("UUID");
    presetData.remove("Pos");
    importedNPC.getEasyNPCPresetData().importPresetData(presetData);

    GameTestHelpers.assertEquals(
        helper,
        "Imported NPC has wrong faction",
        "guards",
        importedNPC.getEasyNPCFactionData().getFactionName());
    GameTestHelpers.assertNotNull(
        helper, "Imported NPC has no scoreboard team", importedNPC.getEntity().getTeam());
    GameTestHelpers.assertEquals(
        helper,
        "Imported NPC is in wrong scoreboard team",
        "guards",
        importedNPC.getEntity().getTeam().getName());
  }
}
