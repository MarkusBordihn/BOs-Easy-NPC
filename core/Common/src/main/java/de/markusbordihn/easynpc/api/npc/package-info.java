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

/**
 * Raw NPC classes that serve as extension points for creating custom NPCs.
 *
 * <p>This package contains auto-generated Raw NPC classes that extend vanilla Minecraft entities
 * and implement the {@link de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase} interface. These
 * classes are designed to be extended by developers who want to create custom NPC types.
 *
 * <h2>Extension Points</h2>
 *
 * <p>All Raw NPC classes follow the pattern: {@code {EntityName}Raw extends {MinecraftEntity}
 * implements EasyNPCBase<{MinecraftEntity}>}
 *
 * <p>Available Raw NPC types include:
 *
 * <ul>
 *   <li>{@link de.markusbordihn.easynpc.api.npc.AllayRaw} - Allay-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.CatRaw} - Cat-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.ChickenRaw} - Chicken-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.CreeperRaw} - Creeper-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.EnderManRaw} - Enderman-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.EvokerRaw} - Evoker-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.FoxRaw} - Fox-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.horse.HorseRaw} - Horse-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.HumanoidRaw} - Generic humanoid NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.HumanoidSlimRaw} - Slim humanoid NPC (Alex model)
 *   <li>{@link de.markusbordihn.easynpc.api.npc.IllusionerRaw} - Illusioner-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.IronGolemRaw} - Iron Golem-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.PathfinderMobRaw} - Generic pathfinder mob NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.PigRaw} - Pig-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.PillagerRaw} - Pillager-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.piglin.PiglinRaw} - Piglin-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.piglin.PiglinBruteRaw} - Piglin Brute-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.piglin.ZombifiedPiglinRaw} - Zombified Piglin-based
 *       NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.skeleton.SkeletonRaw} - Skeleton-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.skeleton.StrayRaw} - Stray-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.skeleton.WitherSkeletonRaw} - Wither Skeleton-based
 *       NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.spider.SpiderRaw} - Spider-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.VexRaw} - Vex-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.villager.VillagerRaw} - Villager-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.villager.ZombieVillagerRaw} - Zombie Villager-based
 *       NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.VindicatorRaw} - Vindicator-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.WitchRaw} - Witch-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.WolfRaw} - Wolf-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.zombie.ZombieRaw} - Zombie-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.zombie.DrownedRaw} - Drowned-based NPC
 *   <li>{@link de.markusbordihn.easynpc.api.npc.zombie.HuskRaw} - Husk-based NPC
 * </ul>
 *
 * <h2>How to Extend</h2>
 *
 * <p>To create a custom NPC:
 *
 * <ol>
 *   <li>Extend one of the Raw NPC classes
 *   <li>Override methods to customize behavior
 *   <li>Register your custom entity with your mod loader
 * </ol>
 *
 * <h2>Example</h2>
 *
 * <pre>{@code
 * public class MyCustomHorse extends HorseRaw {
 *   public MyCustomHorse(EntityType<? extends Horse> entityType, Level level) {
 *     super(entityType, level, HorseSkinVariant.WHITE);
 *   }
 *
 *   @Override
 *   public void tick() {
 *     super.tick();
 *     // Add custom tick behavior
 *   }
 * }
 * }</pre>
 *
 * <h2>Auto-Generation</h2>
 *
 * <p><strong>Warning:</strong> All Raw NPC classes in this package are auto-generated by the {@code
 * generateRawNPCs} Gradle task. Do not edit these files directly. To customize generated classes,
 * use {@code .methods} and {@code .imports} files in the same directory.
 *
 * @see de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase
 * @since 1.0.0
 */
package de.markusbordihn.easynpc.api.npc;
