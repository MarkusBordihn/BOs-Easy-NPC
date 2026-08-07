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
 * Easy NPC Public API for mod developers.
 *
 * <p>This package provides a stable API for developers who want to create custom NPC types or
 * integrate with Easy NPC functionality. The API is designed to allow external mods to extend NPC
 * behavior without depending on internal implementation details.
 *
 * <h2>API Stability</h2>
 *
 * <p>This API is currently in development. While we strive to maintain backwards compatibility,
 * breaking changes may occur in minor versions during the experimental phase. Once the API reaches
 * stable status, it will follow semantic versioning.
 *
 * <h2>Main Components</h2>
 *
 * <ul>
 *   <li>{@link de.markusbordihn.easynpc.api.npc} - Raw NPC classes that can be extended to create
 *       custom NPC types
 *   <li>{@link de.markusbordihn.easynpc.api.action} - Execute NPC actions from code and register
 *       custom action types for presets
 *   <li>{@link de.markusbordihn.easynpc.api.condition} - Register custom condition types for
 *       dialogs and actions
 *   <li>{@link de.markusbordihn.easynpc.api.event} - React to an opened dialog, an executed action
 *       or a changed NPC state
 * </ul>
 *
 * <h2>Usage Example</h2>
 *
 * <p>To create a custom NPC, extend one of the Raw NPC classes:
 *
 * <pre>{@code
 * public class MyCustomHorse extends HorseRaw {
 *   public MyCustomHorse(EntityType<? extends Horse> entityType, Level level) {
 *     super(entityType, level);
 *   }
 *
 *   // Add your custom behavior here
 * }
 * }</pre>
 *
 * <p>Then register your custom NPC entity with your mod loader (Forge/Fabric/NeoForge).
 *
 * <p>To gate a dialog on own data, register a condition once during mod setup and reference its id
 * from the condition of the dialog button:
 *
 * <pre>{@code
 * ConditionRegistry.register(
 *     ResourceLocation.fromNamespaceAndPath("my_mod", "has_quest"),
 *     (conditionDataEntry, serverPlayer, npcContext) -> MyQuests.isActive(serverPlayer));
 * }</pre>
 *
 * <p>A condition that cannot be answered on the client keeps {@code isAvailableOnClient()} at
 * {@code false}; the server then sends a lock for the dialog button instead of letting it look
 * available. An event without an initiator passes {@code null} as the server player. Conditions
 * that require a player must return {@code false} in that case.
 *
 * <p>Use the action handler to execute NPC behavior programmatically:
 *
 * <pre>{@code
 * EasyNPCActionHandler.say(easyNPC, "Good to see you again!");
 * EasyNPCActionHandler.say(easyNPC, List.of("Hello!", "Welcome back!"));
 * EasyNPCActionHandler.showSpeechBubble(easyNPC, "text.my_mod.npc.greeting");
 * EasyNPCActionHandler.sayTo(easyNPC, serverPlayer, "This one is only for you.");
 * }</pre>
 *
 * <p>For events that apply to several players, {@code ActionContext} contains both the initiator
 * and the full audience.
 *
 * <h2>Documentation</h2>
 *
 * <p>For detailed developer documentation, see: <a
 * href="https://github.com/MarkusBordihn/BOs-Easy-NPC/wiki/Developer-Documentation">Developer
 * Documentation Wiki</a>
 *
 * @since 1.0.0
 */
package de.markusbordihn.easynpc.api;
