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

package de.markusbordihn.easynpc.handler;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.faction.FactionNameValidator;
import de.markusbordihn.easynpc.data.saveddata.FactionData;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.scores.TeamColor;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class FactionHandler {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private FactionHandler() {}

  public static boolean setFaction(EasyNPC<?> easyNPC, String factionName) {
    if (easyNPC == null || factionName == null || factionName.isEmpty()) {
      return false;
    }

    FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
    if (factionData == null) {
      log.error("Unable to set faction '{}': no faction data for {}", factionName, easyNPC);
      return false;
    }

    if (FactionData.isInitialized() && !FactionData.get().hasFaction(factionName)) {
      if (!FactionNameValidator.isValid(factionName)) {
        return false;
      }
      if (!FactionData.get().createFaction(factionName)) {
        return false;
      }
    }

    log.debug("Set faction '{}' for {}", factionName, easyNPC);
    factionData.setFactionName(factionName);
    factionData.applyFactionToScoreboard();
    return true;
  }

  public static boolean deleteFaction(String factionName) {
    if (factionName == null
        || factionName.isEmpty()
        || !FactionData.isInitialized()
        || !FactionData.get().removeFaction(factionName)) {
      return false;
    }
    clearLoadedFactionAssignments(factionName);
    return true;
  }

  public static boolean setFactionColor(String factionName, TeamColor color) {
    if (factionName == null
        || factionName.isEmpty()
        || color == null
        || !FactionData.isInitialized()
        || !FactionData.get().setFactionColor(factionName, color)) {
      return false;
    }
    refreshLoadedFactionAssignments(factionName);
    return true;
  }

  public static void clearLoadedFactionAssignments(String factionName) {
    LivingEntityManager.getEasyNPCEntities()
        .filter(easyNPC -> easyNPC != null && !easyNPC.isClientSideInstance())
        .forEach(
            easyNPC -> {
              FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
              if (factionData != null && factionName.equals(factionData.getFactionName())) {
                factionData.setFactionName("");
                factionData.applyFactionToScoreboard();
              }
            });
  }

  public static void refreshLoadedFactionAssignments(String factionName) {
    LivingEntityManager.getEasyNPCEntities()
        .filter(easyNPC -> easyNPC != null && !easyNPC.isClientSideInstance())
        .forEach(
            easyNPC -> {
              FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
              if (factionData != null && factionName.equals(factionData.getFactionName())) {
                factionData.applyFactionToScoreboard();
              }
            });
  }

  public static boolean removeFaction(EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return false;
    }

    FactionDataCapable<?> factionData = easyNPC.getEasyNPCFactionData();
    if (factionData == null) {
      return false;
    }

    log.debug("Remove faction '{}' from {}", factionData.getFactionName(), easyNPC);
    factionData.setFactionName("");
    factionData.applyFactionToScoreboard();
    return true;
  }

  public static boolean isHostile(String factionName, LivingEntity targetEntity) {
    if (factionName == null
        || factionName.isEmpty()
        || targetEntity == null
        || !FactionData.isInitialized()) {
      return false;
    }
    return FactionData.get().isHostile(factionName, getTargetGroupName(targetEntity));
  }

  public static boolean canBypassInvulnerability(LivingEntity attacker, LivingEntity targetEntity) {
    if (attacker == null
        || !(targetEntity instanceof EasyNPC<?> targetEasyNPC)
        || !targetEntity.isAlive()) {
      return false;
    }

    AttributeDataCapable<?> targetAttributeData = targetEasyNPC.getEasyNPCAttributeData();
    if (targetAttributeData == null
        || targetAttributeData.getEntityAttributes() == null
        || !targetAttributeData
            .getEntityAttributes()
            .getCombatAttributes()
            .isAttackableByFactions()) {
      return false;
    }

    String attackerGroupName = getTargetGroupName(attacker);
    return attackerGroupName != null
        && !attackerGroupName.isEmpty()
        && isHostile(attackerGroupName, targetEntity);
  }

  public static String getTargetGroupName(LivingEntity targetEntity) {
    if (targetEntity instanceof EasyNPC<?> targetEasyNPC) {
      FactionDataCapable<?> targetFactionData = targetEasyNPC.getEasyNPCFactionData();
      if (targetFactionData != null && targetFactionData.hasFactionName()) {
        return targetFactionData.getFactionName();
      }
    }
    return targetEntity.getTeam() != null ? targetEntity.getTeam().getName() : null;
  }
}
