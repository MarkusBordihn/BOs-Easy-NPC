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

package de.markusbordihn.easynpc.data.objective.factory;

import static de.markusbordihn.easynpc.data.objective.factory.BuiltInObjectiveFactories.registerTarget;

import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.handler.FactionHandler;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.player.Player;

final class TargetObjectiveFactories {

  private TargetObjectiveFactories() {}

  static void registerFactories() {
    registerTarget(
        ObjectiveType.ATTACK_HOSTILE_FACTIONS,
        new NearestAttackableTargetFactory(
            LivingEntity.class,
            (objectiveDataEntry, easyNPC) -> {
              Mob mob = easyNPC.getMob();
              return targetEntity -> {
                FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
                return factionData != null
                    && !mob.isAlliedTo(targetEntity)
                    && FactionHandler.isHostile(factionData.getFactionName(), targetEntity);
              };
            }));

    registerTarget(
        ObjectiveType.ATTACK_PLAYER_BY_NAME,
        new NearestAttackableTargetFactory(
            Player.class,
            (objectiveDataEntry, easyNPC) -> {
              String targetPlayerName = objectiveDataEntry.getTargetPlayerName();
              if (targetPlayerName == null || targetPlayerName.isEmpty()) {
                return null;
              }

              Mob mob = easyNPC.getMob();
              return targetEntity ->
                  targetEntity.getName().getString().equals(targetPlayerName)
                      && !mob.isAlliedTo(targetEntity);
            }));

    registerTarget(
        ObjectiveType.ATTACK_ENTITY_BY_TEAM,
        new NearestAttackableTargetFactory(
            LivingEntity.class,
            (objectiveDataEntry, easyNPC) -> {
              String targetTeamName = objectiveDataEntry.getTargetTeamName();
              if (targetTeamName == null || targetTeamName.isEmpty()) {
                return null;
              }

              Mob mob = easyNPC.getMob();
              return targetEntity ->
                  targetEntity.getTeam() != null
                      && targetEntity.getTeam().getName().equals(targetTeamName)
                      && !mob.isAlliedTo(targetEntity);
            }));

    registerTarget(
        ObjectiveType.ATTACK_ENTITY_BY_TAG,
        new NearestAttackableTargetFactory(
            LivingEntity.class,
            (objectiveDataEntry, easyNPC) -> {
              String targetEntityTag = objectiveDataEntry.getTargetEntityTag();
              if (targetEntityTag == null || targetEntityTag.isEmpty()) {
                return null;
              }

              Mob mob = easyNPC.getMob();
              return targetEntity ->
                  targetEntity.entityTags().contains(targetEntityTag)
                      && !mob.isAlliedTo(targetEntity);
            }));

    registerTarget(
        ObjectiveType.ATTACK_ENTITY_BY_UUID,
        new NearestAttackableTargetFactory(
            LivingEntity.class,
            (objectiveDataEntry, easyNPC) -> {
              if (objectiveDataEntry.getTargetEntityUUID() == null) {
                return null;
              }

              Mob mob = easyNPC.getMob();
              return targetEntity ->
                  targetEntity.getUUID().equals(objectiveDataEntry.getTargetEntityUUID())
                      && !mob.isAlliedTo(targetEntity);
            }));
  }
}
