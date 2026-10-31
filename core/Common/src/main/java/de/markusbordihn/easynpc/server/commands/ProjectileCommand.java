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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.builder.RequiredArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.attack.ProjectileData;
import de.markusbordihn.easynpc.data.attack.ProjectileHitEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileImpactEffect;
import de.markusbordihn.easynpc.data.attack.ProjectileTrailParticle;
import de.markusbordihn.easynpc.data.attack.ProjectileType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.ProjectileHandler;
import java.util.Collection;
import java.util.function.UnaryOperator;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.IdentifierArgument;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.server.permissions.Permissions;

public class ProjectileCommand extends Command {

  private static final String LEVEL_ARG = "level";
  private static final String SECONDS_ARG = "seconds";

  private ProjectileCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    RequiredArgumentBuilder<CommandSourceStack, ?> npcTargetsArgument =
        Commands.argument(NPC_TARGETS_ARG, EasyNPCArgument.npc());

    npcTargetsArgument
        .then(
            Commands.literal("enabled")
                .then(
                    Commands.argument(VALUE_ARG, BoolArgumentType.bool())
                        .executes(
                            context -> {
                              boolean enabled = BoolArgumentType.getBool(context, VALUE_ARG);
                              return updateProjectileData(
                                  context,
                                  "enabled",
                                  String.valueOf(enabled),
                                  projectileData -> projectileData.withEnabled(enabled));
                            })))
        .then(
            floatSetting(
                "damage",
                ProjectileData.MINIMUM_DAMAGE,
                ProjectileData.MAXIMUM_DAMAGE,
                ProjectileData::withDamage))
        .then(
            floatSetting(
                "velocity",
                ProjectileData.MINIMUM_VELOCITY,
                ProjectileData.MAXIMUM_VELOCITY,
                ProjectileData::withVelocity))
        .then(
            floatSetting(
                "inaccuracy",
                ProjectileData.MINIMUM_INACCURACY,
                ProjectileData.MAXIMUM_INACCURACY,
                ProjectileData::withInaccuracy))
        .then(
            floatSetting(
                "explosion_power",
                ProjectileData.MINIMUM_EXPLOSION_POWER,
                ProjectileData.MAXIMUM_EXPLOSION_POWER,
                ProjectileData::withExplosionPower))
        .then(
            integerSetting(
                "knockback",
                ProjectileData.MINIMUM_KNOCKBACK,
                ProjectileData.MAXIMUM_KNOCKBACK,
                ProjectileData::withKnockback))
        .then(
            integerSetting(
                "shot_count",
                ProjectileData.MINIMUM_SHOT_COUNT,
                ProjectileData.MAXIMUM_SHOT_COUNT,
                ProjectileData::withShotCount))
        .then(
            Commands.literal("item")
                .then(
                    Commands.argument(ITEM_ARG, IdentifierArgument.id())
                        .suggests(
                            (context, suggestionsBuilder) ->
                                SharedSuggestionProvider.suggestResource(
                                    BuiltInRegistries.ITEM.keySet(), suggestionsBuilder))
                        .executes(
                            context ->
                                setProjectileItem(
                                    context, IdentifierArgument.getId(context, ITEM_ARG)))))
        .then(
            Commands.literal("reset")
                .executes(
                    context ->
                        updateProjectileData(
                            context,
                            "settings",
                            "default",
                            projectileData -> ProjectileData.DEFAULT)))
        .then(typeLiteral())
        .then(trailLiteral())
        .then(impactLiteral())
        .then(hitEffectLiteral());

    return Commands.literal("projectile")
        .requires(
            commandSourceStack ->
                commandSourceStack.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
        .then(npcTargetsArgument);
  }

  private static LiteralArgumentBuilder<CommandSourceStack> typeLiteral() {
    LiteralArgumentBuilder<CommandSourceStack> typeLiteral = Commands.literal("type");
    for (ProjectileType projectileType : ProjectileType.values()) {
      typeLiteral.then(
          Commands.literal(projectileType.getSerializedName())
              .executes(
                  context ->
                      updateProjectileData(
                          context,
                          "type",
                          projectileType.getSerializedName(),
                          projectileData -> projectileData.withProjectileType(projectileType))));
    }
    return typeLiteral;
  }

  private static LiteralArgumentBuilder<CommandSourceStack> trailLiteral() {
    LiteralArgumentBuilder<CommandSourceStack> trailLiteral = Commands.literal("trail");
    for (ProjectileTrailParticle trailParticle : ProjectileTrailParticle.values()) {
      trailLiteral.then(
          Commands.literal(trailParticle.getSerializedName())
              .executes(
                  context ->
                      updateProjectileData(
                          context,
                          "trail",
                          trailParticle.getSerializedName(),
                          projectileData -> projectileData.withTrailParticle(trailParticle))));
    }
    return trailLiteral;
  }

  private static LiteralArgumentBuilder<CommandSourceStack> impactLiteral() {
    LiteralArgumentBuilder<CommandSourceStack> impactLiteral = Commands.literal("impact");
    for (ProjectileImpactEffect impactEffect : ProjectileImpactEffect.values()) {
      impactLiteral.then(
          Commands.literal(impactEffect.getSerializedName())
              .executes(
                  context ->
                      updateProjectileData(
                          context,
                          "impact",
                          impactEffect.getSerializedName(),
                          projectileData -> projectileData.withImpactEffect(impactEffect))));
    }
    return impactLiteral;
  }

  private static LiteralArgumentBuilder<CommandSourceStack> hitEffectLiteral() {
    LiteralArgumentBuilder<CommandSourceStack> hitEffectLiteral = Commands.literal("hit_effect");
    for (ProjectileHitEffect hitEffect : ProjectileHitEffect.values()) {
      hitEffectLiteral.then(
          Commands.literal(hitEffect.getSerializedName())
              .executes(
                  context ->
                      updateProjectileData(
                          context,
                          "hit effect",
                          hitEffect.getSerializedName(),
                          projectileData -> projectileData.withHitEffect(hitEffect)))
              .then(
                  Commands.argument(
                          LEVEL_ARG,
                          IntegerArgumentType.integer(
                              ProjectileData.MINIMUM_HIT_EFFECT_LEVEL,
                              ProjectileData.MAXIMUM_HIT_EFFECT_LEVEL))
                      .then(
                          Commands.argument(
                                  SECONDS_ARG,
                                  IntegerArgumentType.integer(
                                      ProjectileData.MINIMUM_HIT_EFFECT_SECONDS,
                                      ProjectileData.MAXIMUM_HIT_EFFECT_SECONDS))
                              .executes(
                                  context -> {
                                    int level = IntegerArgumentType.getInteger(context, LEVEL_ARG);
                                    int seconds =
                                        IntegerArgumentType.getInteger(context, SECONDS_ARG);
                                    return updateProjectileData(
                                        context,
                                        "hit effect",
                                        hitEffect.getSerializedName()
                                            + " level "
                                            + level
                                            + " for "
                                            + seconds
                                            + " s",
                                        projectileData ->
                                            projectileData
                                                .withHitEffect(hitEffect)
                                                .withHitEffectLevel(level)
                                                .withHitEffectSeconds(seconds));
                                  }))));
    }
    return hitEffectLiteral;
  }

  private static ArgumentBuilder<CommandSourceStack, ?> floatSetting(
      String settingName, float minimum, float maximum, FloatSettingUpdate floatSettingUpdate) {
    return Commands.literal(settingName)
        .then(
            Commands.argument(VALUE_ARG, FloatArgumentType.floatArg(minimum, maximum))
                .executes(
                    context -> {
                      float value = FloatArgumentType.getFloat(context, VALUE_ARG);
                      return updateProjectileData(
                          context,
                          settingName,
                          String.valueOf(value),
                          projectileData -> floatSettingUpdate.apply(projectileData, value));
                    }));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> integerSetting(
      String settingName, int minimum, int maximum, IntegerSettingUpdate integerSettingUpdate) {
    return Commands.literal(settingName)
        .then(
            Commands.argument(VALUE_ARG, IntegerArgumentType.integer(minimum, maximum))
                .executes(
                    context -> {
                      int value = IntegerArgumentType.getInteger(context, VALUE_ARG);
                      return updateProjectileData(
                          context,
                          settingName,
                          String.valueOf(value),
                          projectileData -> integerSettingUpdate.apply(projectileData, value));
                    }));
  }

  private static int setProjectileItem(
      CommandContext<CommandSourceStack> context, Identifier projectileItem)
      throws CommandSyntaxException {
    if (!BuiltInRegistries.ITEM.containsKey(projectileItem)) {
      return sendFailureMessage(context.getSource(), "Unknown item " + projectileItem + "!");
    }

    return updateProjectileData(
        context,
        "item",
        projectileItem.toString(),
        projectileData -> projectileData.withProjectileItem(projectileItem));
  }

  private static int updateProjectileData(
      CommandContext<CommandSourceStack> context,
      String settingName,
      String value,
      UnaryOperator<ProjectileData> projectileDataUpdate)
      throws CommandSyntaxException {
    Collection<? extends EasyNPC<?>> easyNPCs =
        EasyNPCArgument.getEntitiesWithAccess(context, NPC_TARGETS_ARG);
    int changedEntities = 0;
    for (EasyNPC<?> easyNPC : easyNPCs) {
      if (ProjectileHandler.setProjectileData(
          easyNPC, projectileDataUpdate.apply(ProjectileHandler.getProjectileData(easyNPC)))) {
        changedEntities++;
      }
    }

    if (changedEntities == 0) {
      return sendFailureMessage(
          context.getSource(), "Projectile " + settingName + " was not changed!");
    }

    return sendSuccessMessage(
        context.getSource(),
        "Set projectile "
            + settingName
            + " to "
            + value
            + " for "
            + changedEntities
            + " of "
            + easyNPCs.size()
            + " Easy NPCs!");
  }

  @FunctionalInterface
  private interface FloatSettingUpdate {
    ProjectileData apply(ProjectileData projectileData, float value);
  }

  @FunctionalInterface
  private interface IntegerSettingUpdate {
    ProjectileData apply(ProjectileData projectileData, int value);
  }
}
